-- | Test runner for the agent interaction frontend
--   (@agda --interaction-agent@).
--
--   Each test consists of a script @X.jsonl@ under @test\/Agent@, a
--   golden file @X.out@, and one of two fixtures: an Agda file
--   @X.agda@, or a directory @X\/@ holding a whole project with its own
--   @.agda-lib@.  Either is copied to a temporary directory, so scripts
--   may edit it (which is how resynchronisation is tested) without
--   disturbing the repository.
--
--   A project fixture keeps its interface files, since they are what
--   the name index is built from; a single file is checked from
--   scratch every time.
--
--   Script lines are, in order of precedence:
--
--   * empty, or starting with @#@: a comment;
--   * @!file PATH@: make @PATH@, relative to the copy, the file that
--     @%FILE%@ and the directives below refer to.  A single-file
--     fixture needs no such line;
--   * @!rm PATH@: delete @PATH@ from the copy, which is how a module
--     the index has and the project no longer does is reached;
--   * @!edit OLD ==> NEW@: replace the first occurrence of @OLD@ in the
--     Agda file by @NEW@;
--   * @!source@: copy the Agda file into the transcript, so that what a
--     mutating method wrote is a golden assertion rather than a hand
--     check;
--   * @!unchanged@: assert that the Agda file still has the contents it
--     had when it was last written out by @!edit@ or shown by
--     @!source@ — that is, that the requests in between wrote nothing;
--   * anything else: a JSON request, sent with a fresh id.  Occurrences
--     of @%FILE%@ are replaced by the path of the copy.
--
--   The golden file records, for each request, the terminal line with
--   its keys sorted and its @text@ fields spelled out.  Progress events
--   are dropped, as how many there are depends on timing.

module Agent.Tests ( tests, clientTestFilter ) where

import Control.Concurrent           ( forkIO, threadDelay )
import Control.Concurrent.MVar      ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception            ( SomeException, bracket, try )
import Control.Monad                ( forM_ )

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as B
import Data.Char                    ( isDigit )
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List                    ( isInfixOf, isPrefixOf, sort, sortOn )
import Data.Maybe                   ( fromMaybe, isJust )
import Data.Text                    ( Text )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import System.Directory
import System.Environment           ( getEnvironment, lookupEnv )
import System.Exit                ( ExitCode(..) )
import System.FilePath hiding ( normalise )
import System.IO
import System.IO.Temp               ( withSystemTempDirectory )
import System.Process
import System.Timeout               ( timeout )

import Test.Tasty                   ( TestTree, testGroup )
import Test.Tasty.HUnit             ( assertBool, testCase )
import Test.Tasty.Silver.Advanced qualified as Silver
import Test.Tasty.Silver.Filter     ( RegexFilter( RFInclude ) )

import Utils
  ( getAgdaBin, readFileMaybeText, textDiffWithTouch, writeTextFile )

import Agda.Version qualified

testDir :: FilePath
testDir = "test" </> "Agent"

-- | How long a whole script may take.  Scripts that exercise the
--   per-request timeout arm their own, much smaller, ones.
scriptTimeout :: Int
scriptTimeout = 300 * 1000000

tests :: IO TestTree
tests = do
  scripts <- filter ((== ".jsonl") . takeExtension) <$> listDirectory testDir
  agda    <- getAgdaBin <$> getEnvironment
  cases   <- mapM fixtureOf (sort scripts)
  return $ testGroup "Agent" $
    map (uncurry (mkScriptTest agda)) cases ++ [ clientTest agda ]
  where
  fixtureOf script = do
    isLib <- doesDirectoryExist (testDir </> takeBaseName script)
    return (script, if isLib then Library else SingleFile)

------------------------------------------------------------------------
-- * Script tests

-- | What a script is run against.
data Fixture
  = SingleFile
      -- ^ @X.agda@: one module, checked from scratch.
  | Library
      -- ^ @X\/@: a project with its own @.agda-lib@, and so with a
      --   build directory the name index can live in.
  deriving Eq

mkScriptTest :: FilePath -> FilePath -> Fixture -> TestTree
mkScriptTest agda script fixture =
  Silver.goldenTestIO1
    name
    (readFileMaybeText golden)
    runScript
    (textDiffWithTouch touched)
    (pure . Silver.ShowText)
    (Just $ writeTextFile golden)
  where
  name   = takeBaseName script
  golden = testDir </> name <.> "out"
  -- Sort the test up in the next run, as the golden framework asks; a
  -- project fixture has no single file to touch, so its script serves.
  touched = case fixture of
    SingleFile -> testDir </> name <.> "agda"
    Library    -> testDir </> script

  runScript = do
    script' <- lines <$> readFileUtf8 (testDir </> script)
    withSystemTempDirectory "agda-client-test" $ \ dir -> do
      file <- case fixture of
        Library    -> Nothing <$ copyTree (testDir </> name) dir
        SingleFile -> do
          let file = dir </> name <.> "agda"
          writeFileUtf8 file =<< readFileUtf8 (testDir </> name <.> "agda")
          return $ Just file
      result <- timeout scriptTimeout $
        withAgent agda dir fixture $ \ session ->
          runScriptLines session dir file script'
      return $ normalise dir $ case result of
        Nothing -> "the script did not finish in time\n"
        Just ls -> T.unlines ls

-- | Copy a fixture project into the temporary directory it is run in.
copyTree :: FilePath -> FilePath -> IO ()
copyTree from to = do
  createDirectoryIfMissing True to
  entries <- listDirectory from
  forM_ entries $ \ e -> do
    isDir <- doesDirectoryExist (from </> e)
    if isDir then copyTree (from </> e) (to </> e)
             else copyFile  (from </> e) (to </> e)

-- | An Agda process speaking the agent protocol.
data Session = Session { sIn :: Handle, sOut :: Handle }

withAgent :: FilePath -> FilePath -> Fixture -> (Session -> IO a) -> IO a
withAgent agda dir fixture k = bracket start stop $ \ (_, hout, _, session) -> do
  _ <- B.hGetLine hout  -- the ready banner
  k session
  where
  start = do
    (Just hin, Just hout, _, ph) <- createProcess (proc agda args)
      { std_in = CreatePipe, std_out = CreatePipe, cwd = Just dir }
    hSetBinaryMode hin  True
    hSetBinaryMode hout True
    hSetBuffering  hin  LineBuffering
    return (hin, hout, ph, Session hin hout)
  stop (hin, _, ph, _) = do
    _ <- try (hClose hin)          :: IO (Either SomeException ())
    _ <- try (terminateProcess ph) :: IO (Either SomeException ())
    _ <- try (waitForProcess ph)   :: IO (Either SomeException ExitCode)
    return ()
  args = concat
    [ [ "--interaction-agent"
      , "-v0", "-i", ".", "--no-default-libraries", "--color=never"
      ]
      -- A project fixture keeps what it checks: its interface files are
      -- what the name index is built from, and rechecking from scratch
      -- on every load would say nothing about staleness.
    , [ "--ignore-interfaces" | fixture == SingleFile ]
    ]

-- | Interpret the script, collecting the lines of the transcript.
--
--   The file the directives act on is the fixture's own for a single
--   file, and whatever the last @!file@ named for a project.
runScriptLines :: Session -> FilePath -> Maybe FilePath -> [String] -> IO [Text]
runScriptLines session dir = go (1 :: Int) Nothing
  where
  go _ _ _ [] = return []
  go n snapshot file (l : ls) = case dropWhile (== ' ') l of
    ""        -> go n snapshot file ls
    '#' : _   -> go n snapshot file ls
    '!' : cmd -> directive n snapshot file cmd ls
    request   -> case file of
      Nothing -> ("! no file: the script needs a !file directive first" :)
                   <$> go n snapshot file ls
      Just f -> do
        answer <- send n $ replaceAll "%FILE%" f request
        ((header n request : answer) ++) <$> go (n + 1) snapshot file ls

  directive n snapshot file cmd ls = case cmd of
    'f' : 'i' : 'l' : 'e' : ' ' : path ->
      ("! file " <> T.pack path :)
        <$> go n Nothing (Just (dir </> dropWhile (== ' ') path)) ls
    'r' : 'm' : ' ' : path -> do
      removeFile (dir </> dropWhile (== ' ') path)
      ("! removed " <> T.pack path :) <$> go n snapshot file ls
    _ | Nothing <- file ->
      (("! no file: " <> T.pack cmd <> " needs a !file directive first") :)
        <$> go n snapshot file ls
    'e' : 'd' : 'i' : 't' : ' ' : args | Just f <- file -> do
      let (old, new) = splitEdit args
      source <- readFileUtf8 f
      case replaceFirst old new source of
        Nothing -> (("! edit: no occurrence of " <> T.pack old) :)
                     <$> go n snapshot file ls
        Just source' -> do
          writeFileUtf8 f source'
          ("! edited" :) <$> go n (Just source') file ls
    "source" | Just f <- file -> do
      source <- readFileUtf8 f
      let shown = "! source" : map ("  " <>) (T.lines (T.pack source))
      (shown ++) <$> go n (Just source) file ls
    "unchanged" | Just f <- file -> do
      source <- readFileUtf8 f
      let ok = maybe True (== source) snapshot
      ((if ok then "! file unchanged" else "! FILE WAS MODIFIED") :)
        <$> go n snapshot file ls
    _ -> (("! unknown directive: " <> T.pack cmd) :) <$> go n snapshot file ls

  -- Send one request and collect its answer, dropping progress events.
  send n request = do
    BS.hPutStrLn (sIn session) $ case decode (utf8Encode request) of
      Just (Object o) ->
        encode $ Object $ KeyMap.insert (Key.fromText "id") (toJSON n) o
      _ -> utf8Encode request  -- deliberately malformed: pass it on unchanged
    hFlush (sIn session)
    collect
    where
    collect = do
      l <- BS.fromStrict <$> B.hGetLine (sOut session)
      case decode l of
        Nothing -> return [ "unreadable: " <> T.decodeUtf8 (BS.toStrict l) ]
        Just v
          | isJust (look "event" v) -> collect
          | otherwise               -> return $ prettyValue $ scrubProfiles $ withoutId v

  header n request = T.pack $ concat
    [ ">>> ", show n, " "
    , case decode (utf8Encode request) of
        Just v | Just (String m) <- look "method" v -> T.unpack m
        _                                          -> "(malformed request)"
    ]

  withoutId (Object o) = Object $ KeyMap.delete (Key.fromText "id") o
  withoutId v          = v

-- | Timings differ from run to run, so a profile is reduced to its most
--   expensive account, which a fixture can make certain, and the table
--   in a @text@ beside it to its heading.
scrubProfiles :: Value -> Value
scrubProfiles = \case
  Object o -> Object $ KeyMap.mapWithKey scrub $ fmap scrubProfiles o
    where
    hasProfile = KeyMap.member (Key.fromText "profile") o
    scrub k v = case (Key.toText k, v) of
      ("profile", Object p) -> String $ case KeyMap.lookup (Key.fromText "accounts") p of
        Just (Array as) | (a : _) <- foldr (:) [] as
                        , Just (String n) <- look "account" a ->
          "most expensive: " <> n
        _ -> "no accounts"
      ("text", String t) | hasProfile ->
        String $ case T.breakOn "\n\nProfile (" t of
          (before, after) | T.null after -> before
                          | otherwise    -> before <> "\n\nProfile (CPU time; rows elided)"
      _ -> v
  Array a  -> Array $ fmap scrubProfiles a
  v        -> v

look :: Text -> Value -> Maybe Value
look k (Object o) = KeyMap.lookup (Key.fromText k) o
look _ _          = Nothing

-- | @OLD ==> NEW@, with @\n@ and @\t@ spelled out on both sides.
splitEdit :: String -> (String, String)
splitEdit s = case breakOn " ==> " s of
  Just (old, new) -> (unescape old, unescape new)
  Nothing         -> (unescape s, unescape s)
  where
  unescape = \case
    '\\' : 'n' : cs -> '\n' : unescape cs
    '\\' : 't' : cs -> '\t' : unescape cs
    c          : cs -> c    : unescape cs
    []              -> []

------------------------------------------------------------------------
-- * The client

-- | The client test needs to know where @agda-client@ is: the
--   @Makefile@ says so in @AGDA_CLIENT@, which a bare @cabal test@ does
--   not set.  Registered in "Main" against @AGDA_BIN@, like the other
--   tests whose setup the @Makefile@ provides.
clientTestFilter :: [RegexFilter]
clientTestFilter = [ RFInclude "Agent/agda-client" ]

-- | A smoke test for @agda-client@: cold start with Agda's own flags in
--   @load@'s tail, warm reuse, an abort that has to reach the daemon
--   while it is busy, shutdown, and no writes to the source.
--
--   The client is the one @AGDA_CLIENT@ names, else the one beside
--   @agda@ (where an installation puts it; a build directory does not).
--   Where it is missing this fails rather than passing quietly: in the
--   build this test runs under, @agda-client@ not being there is a
--   packaging fault, not a property of the machine.  The build where it
--   may legitimately be missing is filtered out by 'clientTestFilter'.
clientTest :: FilePath -> TestTree
clientTest agda = testCase "agda-client" $ do
  found  <- findExecutable agda
  given  <- lookupEnv "AGDA_CLIENT"
  let cli = fromMaybe (takeDirectory (fromMaybe agda found) </> "agda-client") given
  exists <- doesFileExist cli
  assertBool ("agda-client not found at " ++ cli ++ "; set AGDA_CLIENT") exists
  withSystemTempDirectory "agda-client-cli" $ \ tmp -> do
    source <- readFileUtf8 (testDir </> "Timeout.agda")
    let file = tmp </> "Timeout.agda"
    writeFileUtf8 file source
    environment <- getEnvironment
    let overrides = [ ("AGDA_CLIENT_BIN", agda), ("AGDA_CLIENT_ROOT", tmp) ]
        run args = readCreateProcessWithExitCode
          (proc cli args)
            { cwd = Just tmp
            , env = Just $ overrides ++
                filter ((`notElem` map fst overrides) . fst) environment
            }
          ""
    -- The tail of `load` is Agda's: a long flag the client does not
    -- know reaches Agda as it stands, and `--` hands over one it does
    -- know (--show-implicit is also an option of the client's).
    (_, out1, err1) <- run
      [ "load", "Timeout.agda", "--double-check", "--", "--show-implicit" ]
    assertBool ("cold start did not list the goals:\n" ++ out1 ++ err1) $
      "?0" `isInfixOf` out1
    (_, out2, err2) <- run [ "goals" ]
    assertBool ("warm session did not list the goals:\n" ++ out2 ++ err2) $
      "?0" `isInfixOf` out2

    -- --profile is the client's wherever it stands, and gets a table.
    (_, out3, err3) <- run [ "reload", "--profile" ]
    assertBool ("a profiled reload printed no profile:\n" ++ out3 ++ err3) $
      "Profile (CPU time" `isInfixOf` out3
    (code3, _, err3') <- run [ "--profile", "goals" ]
    assertBool ("--profile was accepted for goals:\n" ++ err3') $
      code3 == ExitFailure 2
    (_, outV, _) <- run [ "--version" ]
    assertBool ("--version printed " ++ show outV) $
      ("agda-client " ++ Agda.Version.version) `isPrefixOf` outV

    -- An abort has to be read while another request is in flight, which
    -- it cannot be if the daemon serves one connection at a time.  The
    -- diverging request carries a timeout of its own, so that a missed
    -- abort fails the test instead of hanging it.
    running <- newEmptyMVar
    _ <- forkIO $ putMVar running =<<
           run [ "-t", "30", "normalise", "loop zero" ]
    threadDelay 2000000
    (_, out4, err4) <- run [ "abort" ]
    assertBool ("abort did not reach a busy daemon:\n" ++ out4 ++ err4) $
      "aborted" `isInfixOf` (out4 ++ err4)
    -- Not merely "aborted": a daemon that serves one connection at a
    -- time would let the request run into its own timeout instead, and
    -- that failure also says the command was aborted.
    (_, out5, err5) <- takeMVar running
    assertBool ("the aborted request did not report the abort:\n" ++ out5 ++ err5) $
      "normalise: aborted" `isInfixOf` (out5 ++ err5)
    (_, out6, err6) <- run [ "goals" ]
    assertBool ("the session did not survive the abort:\n" ++ out6 ++ err6) $
      "?0" `isInfixOf` out6

    (_, out7, err7) <- run [ "stop" ]
    assertBool ("stop did not shut the daemon down:\n" ++ out7 ++ err7) $
      "stopped" `isInfixOf` out7
    source' <- readFileUtf8 file
    assertBool "agda-client modified the source" $ source == source'

------------------------------------------------------------------------
-- * Rendering answers

-- | Canonical rendering of a JSON value: keys sorted, and the fields
--   that carry Agda's own rendering spelled out as indented blocks.
prettyValue :: Value -> [Text]
prettyValue = go ""
  where
  go indent = \case
    Object o
      | KeyMap.null o -> [ indent <> "{}" ]
      | otherwise     -> concat
          [ field indent (Key.toText k) v
          | (k, v) <- sortOn fst (KeyMap.toList o) ]
    Array a
      | null (elems a) -> [ indent <> "[]" ]
      | otherwise      -> concat [ item indent v | v <- elems a ]
    v -> [ indent <> scalar v ]

  field indent k v = case v of
    String t | k `elem` blockFields ->
      (indent <> k <> ":") : map ((indent <> "  ") <>) (T.lines t)
    Object o | KeyMap.null o    -> [ indent <> k <> ": {}" ]
    Array a  | null (elems a)   -> [ indent <> k <> ": []" ]
    _ | Just c <- compact v     -> [ indent <> k <> ": " <> c ]
    Object{} -> (indent <> k <> ":") : go (indent <> "  ") v
    Array{}  -> (indent <> k <> ":") : go (indent <> "  ") v
    _        -> [ indent <> k <> ": " <> scalar v ]

  item indent v = case v of
    _ | Just c <- compact v -> [ indent <> "- " <> c ]
    Object{} -> case go (indent <> "  ") v of
      []     -> [ indent <> "-" ]
      x : xs -> (indent <> "- " <> T.drop (T.length indent + 2) x) : xs
    _ -> [ indent <> "- " <> scalar v ]

  blockFields = [ "text", "message" ]
  scalar = T.decodeUtf8 . BS.toStrict . encode
  elems  = foldr (:) []

-- | Positions and ranges on one line.
--
--   They are half of everything the protocol says, and nine lines
--   each is what made these transcripts unreadable.  A position is
--   @LINE:COL\@OFFSET@, a range is @FILE START-END@.
compact :: Value -> Maybe Text
compact v = case v of
  Object o -> case sortOn fst (KeyMap.toList o) of
    [("end", e), ("file", f), ("start", b)] -> do
      file <- case f of
        String t -> Just t
        Null     -> Just "?"
        _        -> Nothing
      b' <- position b
      e' <- position e
      return $ file <> " " <> b' <> "-" <> e'
    _ -> position v
  _ -> Nothing
  where
  position (Object o) = case sortOn fst (KeyMap.toList o) of
    [("col", c), ("line", l), ("offset", n)] -> do
      l' <- number l
      c' <- number c
      n' <- number n
      return $ l' <> ":" <> c' <> "@" <> n'
    _ -> Nothing
  position _ = Nothing

  number (Number n) = Just $ T.pack $ show (truncate n :: Integer)
  number _          = Nothing

-- | Hide what varies between machines and builds: the temporary
--   directory, the Agda version, the paths of things outside the
--   fixture, and the time an index was built at.
normalise :: FilePath -> Text -> Text
normalise dir =
  T.unlines . map (scrubPath . scrubTime . scrubMs . scrubVersion . scrubImpossible) . T.lines
  . T.replace (T.pack buildDir) "_build/<version>/agda"
  . T.replace (T.pack dir) ""
  . T.replace (T.pack (dir ++ [pathSeparator])) ""
  where
  buildDir = "_build" </> Agda.Version.version </> "agda"

  scrubVersion l
    | Just rest <- T.stripPrefix "agdaVersion: " (T.stripStart l) =
        indentOf l <> "agdaVersion: " <> version rest
    | Just _ <- T.stripPrefix "agda: " (T.stripStart l) =
        indentOf l <> "agda:     <version>"
    | otherwise = l
  version _ = "\"<version>\""
  indentOf l = T.takeWhile (== ' ') l

  -- When the index was built is a timestamp, in the answer and in the
  -- line of text that renders it.
  scrubTime l
      -- Only a time: `builtAt: null`, which is how an answer says there
      -- is no index at all, has to survive.
    | Just _ <- T.stripPrefix "builtAt: \"" (T.stripStart l) =
        indentOf l <> "builtAt: \"<time>\""
    | Just _ <- T.stripPrefix "index:     built " (T.stripStart l) =
        indentOf l <> "index:     built <time>"
    | otherwise = l

  -- CPU time, as a timeout reports what it was checking: "after 2914 ms".
  scrubMs = T.pack . go . T.unpack
    where
    go s = case span isDigit s of
      (ds@(_ : _), rest) | " ms" `isPrefixOf` rest -> 'N' : go rest
                         | otherwise               -> ds ++ go rest
      ([], c : cs) -> c : go cs
      ([], [])     -> []

  -- A call the frontend cannot print reports where in Agda's own
  -- sources the placeholder it tripped over sits, package id and all.
  scrubImpossible l = case T.breakOn marker l of
    (before, rest) | not (T.null rest) -> before <> marker <> "<internal error>)"
    _ -> l
    where marker = "could not be printed: "

  -- Everything inside the fixture is relative by now, so a path that is
  -- still absolute is one this machine put there: the primitive
  -- library's, wherever Agda was installed.
  scrubPath l = case T.breakOn ": \"/" l of
    (before, rest) | not (T.null rest) -> before <> ": \"<path>\""
    _ -> case T.breakOn "- \"/" l of
      (before, rest) | not (T.null rest) -> before <> "- \"<path>\""
      _ -> l

------------------------------------------------------------------------
-- * Small helpers

-- | The scripts contain Unicode, and Agda reads its stdin as UTF-8.
utf8Encode :: String -> BS.ByteString
utf8Encode = BS.fromStrict . T.encodeUtf8 . T.pack

readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = withFile f ReadMode $ \ h -> do
  hSetEncoding h utf8
  s <- hGetContents h
  length s `seq` return s

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 f s = withFile f WriteMode $ \ h -> do
  hSetEncoding h utf8
  hPutStr h s

replaceFirst :: String -> String -> String -> Maybe String
replaceFirst old new s = do
  (before, after) <- breakOn old s
  return $ before ++ new ++ after

replaceAll :: String -> String -> String -> String
replaceAll old new s = case breakOn old s of
  Just (before, after) -> before ++ new ++ replaceAll old new after
  Nothing              -> s

-- | Split on the first occurrence of the needle, which is dropped.
breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go ""
  where
  go _ [] = Nothing
  go acc s@(c : cs)
    | needle `isPrefixOf` s = Just (reverse acc, drop (length needle) s)
    | otherwise             = go (c : acc) cs
