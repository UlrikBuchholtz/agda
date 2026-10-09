-- | @agda-client@: a one-shot command line client for
--   @agda --interaction-agent@.
--
--   Type checking is expensive, so the Agda session has to outlive a
--   single command invocation.  This client lets people, scripts and
--   coding agents issue commands against a persistent session:
--   it attaches to (or starts) a per-project daemon,
--   sends one request, prints a compact rendering, and exits.
--
--   The daemon is this same executable running with the hidden
--   @--daemon@ flag: it owns a Unix socket, spawns
--   @agda --interaction-agent@ behind it and routes JSON lines between
--   clients and that process by request id.  The Agda process itself
--   stays a plain stdio program, so it remains directly drivable by
--   tests and other clients.

module Main (main) where

import Control.Concurrent        ( forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad             ( forM_, forever, unless, void, when )

import Data.Aeson hiding ( Options, defaultOptions )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bits                 ( xor, (.&.), shiftR )
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char                 ( isDigit, ord )
import Data.List                 ( isPrefixOf, stripPrefix )
import Data.Map.Strict           ( Map )
import Data.Map.Strict qualified as Map
import Data.Maybe                ( fromMaybe )
import Data.Text                 ( Text )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Word                 ( Word64 )

import Network.Socket

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Info               ( os )
import System.IO
import System.PosixCompat.Files  ( setFileMode )
import System.Process
import System.Timeout            ( timeout )

import Agda.VersionCommit        ( versionWithCommitInfo )

------------------------------------------------------------------------
-- * Configuration

-- | Bump when the client and daemon stop understanding each other; it
--   is part of the socket name, so old daemons are simply not reused.
clientVersion :: String
clientVersion = "1"

agdaBin :: IO FilePath
agdaBin = fromMaybe "agda" <$> lookupEnv "AGDA_CLIENT_BIN"

-- | Extra arguments passed to the Agda process, whitespace separated.
agdaArgs :: IO [String]
agdaArgs = maybe [] words <$> lookupEnv "AGDA_CLIENT_ARGS"

-- | Seconds the daemon stays alive without requests.
idleTimeout :: IO Int
idleTimeout = do
  s <- lookupEnv "AGDA_CLIENT_IDLE"
  return $ fromMaybe (4 * 3600) (readMaybeInt =<< s)

-- | Where the daemon for a project root listens.
--
--   The name is keyed by the project root, by the identity of the
--   Agda binary (path and modification time), and by the arguments the
--   daemon starts it with, so a rebuilt Agda — or a different
--   @AGDA_CLIENT_ARGS@ — gets a fresh daemon instead of a version
--   mismatch or a session running under the arguments of a previous
--   one.
--
--   The socket and its log live in @$XDG_RUNTIME_DIR@, which is
--   private to the user; where that is not set the cache directory
--   serves, since it is under the user's home.  A world-writable
--   directory would not do: the name is a hash of public information,
--   so anybody who can create that path first can answer in the
--   daemon's stead.  The directory is restricted to its owner in any
--   case.
socketPath :: FilePath -> IO FilePath
socketPath root = do
  bin   <- agdaBin
  extra <- agdaArgs
  found <- findExecutable bin
  stamp <- case found of
    Nothing -> return ""
    Just f  -> (show <$> getModificationTime f) `catch` \ (_ :: IOException) -> return ""
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  dir <- case runtime of
    Just d  -> return $ d </> "agda-client"
    Nothing -> getXdgDirectory XdgCache "agda-client"
  let key = hashString $ unwords $
        [root, bin, fromMaybe bin found, stamp, clientVersion] ++ extra
  createDirectoryIfMissing True dir
  -- Owner only.  ('setPermissions' will not do: it leaves the group and
  -- other bits as it found them.)
  setFileMode dir 0o700 `catch` \ (_ :: IOException) -> return ()
  let path = dir </> key <.> "sock"
      size = B.length $ T.encodeUtf8 $ T.pack path
  -- Checked here rather than left to 'connect', which would report it
  -- as a crash rather than as something the user can do anything about.
  when (size >= unixPathMax) $ die' $ concat
    [ "the daemon's socket would be ", path, ", which at ", show size
    , " bytes is too long for a Unix domain socket (the limit is "
    , show (unixPathMax - 1), "); point XDG_RUNTIME_DIR at a shorter "
    , "directory that only you can write to" ]
  return path

-- | The size of @sun_path@, terminating NUL included: 108 on Linux,
--   104 on the BSDs and macOS.
unixPathMax :: Int
unixPathMax = if os == "linux" then 108 else 104

-- | FNV-1a, hex encoded: short enough to keep the socket path within
--   the length limit of @AF_UNIX@ addresses.
hashString :: String -> String
hashString = hex . foldl step 0xcbf29ce484222325
  where
  step :: Word64 -> Char -> Word64
  step h c = (h `xor` fromIntegral (ord c `mod` 256)) * 0x100000001b3
  hex :: Word64 -> String
  hex w = [ "0123456789abcdef" !! fromIntegral ((w `shiftR` (4 * i)) .&. 15)
          | i <- [15, 14 .. 0 :: Int] ]

-- | The project root: the innermost enclosing directory with an
--   @.agda-lib@ file, else the current directory.
findRoot :: IO FilePath
findRoot = lookupEnv "AGDA_CLIENT_ROOT" >>= \case
  Just r  -> canonicalizePath r
  Nothing -> do
    cwd <- getCurrentDirectory
    go cwd cwd
  where
  go orig dir = do
    entries <- listDirectory dir `catch` \ (_ :: IOException) -> return []
    if any ((== ".agda-lib") . takeExtension) entries then return dir else do
      let up = takeDirectory dir
      if up == dir then return orig else go orig up

------------------------------------------------------------------------
-- * Entry point

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- The daemon is reached through a Unix domain socket; without those
  -- there is nothing to attach to, and saying so beats spawning a
  -- daemon that cannot listen and then waiting a minute for it.
  unless isUnixDomainSocketAvailable $ die'
    "this platform has no Unix domain sockets, which agda-client needs to \
    \keep a session alive between invocations; drive `agda \
    \--interaction-agent` directly instead"
  getArgs >>= \case
    "--daemon" : root : sock : _ -> daemon root sock
    args                         -> client args

------------------------------------------------------------------------
-- * The client

data Options = Options
  { optJson     :: Bool
  , optTimeout  :: Maybe Double  -- ^ Seconds; 'Nothing' means the default.
  , optNoTimeout:: Bool
  , optNorm     :: Maybe String
  , optMode     :: Maybe String
  , optRoot     :: Maybe FilePath
  , optRevision :: Maybe Int
  , optForce    :: Bool
  , optPmLambda :: Bool
  , optImplicit :: Bool
  , optIrrelevant :: Bool
  , optIn       :: Maybe String   -- ^ @find@: restrict to a module prefix.
  , optGlob     :: Bool           -- ^ @find@: read @*@ and @?@ as wildcards.
  , optKinds    :: [String]       -- ^ @find@: restrict to these kinds of name.
  , optType     :: Maybe String   -- ^ @find@: names the type must mention.
  , optLimit    :: Maybe Int      -- ^ @find@: how many hits to print.
  , optProfile  :: [String]       -- ^ @load@, @reload@: profiling options.
  }

defaultOptions :: Options
defaultOptions =
  Options False Nothing False Nothing Nothing Nothing Nothing False False
          False False Nothing False [] Nothing Nothing []

client :: [String] -> IO ()
client argv = do
  (opts, positional) <- parseArgs defaultOptions [] argv
  case positional of
    [] -> putStr usage >> exitSuccess
    verb : args
      | verb `elem` ["-h", "--help", "help"] -> putStr usage >> exitSuccess
      | otherwise -> do
          root <- maybe findRoot canonicalizePath (optRoot opts)
          request <- buildRequest opts root verb args
          sock <- socketPath root
          runRequest opts root sock verb request

-- | Split the command line into the client's own options, which may
--   come anywhere, and the positional arguments: the verb and its own.
--
--   Two things get past the unknown-flag check, so that @load@'s
--   documented @AGDA-ARG...@ tail can reach Agda: a long flag the
--   client does not know, once the verb and its file are in
--   (@load FILE --without-K@), and everything after a @--@, which ends
--   the client's options for good (@load FILE -- --show-implicit@, for
--   a flag the client would otherwise have taken for itself).
parseArgs :: Options -> [String] -> [String] -> IO (Options, [String])
parseArgs opts acc = \case
  [] -> return (opts, reverse acc)
  "--" : rest -> return (opts, reverse acc ++ rest)
  a : rest -> case a of
    "-h"                -> putStr usage >> exitSuccess
    "--help"            -> putStr usage >> exitSuccess
    "--version"         -> do
      putStrLn $ "agda-client " ++ versionWithCommitInfo
      exitSuccess
    "--profile"         -> parseArgs opts{ optProfile = optProfile opts ++ ["definitions"] } acc rest
    _ | Just p <- stripPrefix "--profile=" a ->
                           parseArgs opts{ optProfile = optProfile opts ++ [p] } acc rest
    "--json"            -> parseArgs opts{ optJson = True } acc rest
    "--force"           -> parseArgs opts{ optForce = True } acc rest
    "--glob"            -> parseArgs opts{ optGlob = True } acc rest
    "--in"              -> withArg rest $ \ v r ->
                             parseArgs opts{ optIn = Just v } acc r
    "--kind"            -> withArg rest $ \ v r ->
                             parseArgs opts{ optKinds = optKinds opts ++ [v] } acc r
    "--type"            -> withArg rest $ \ v r ->
                             parseArgs opts{ optType = Just v } acc r
    "--limit"           -> withArg rest $ \ v r ->
                             parseArgs opts{ optLimit = readMaybeInt v } acc r
    "-p"                -> parseArgs opts{ optPmLambda = True } acc rest
    "--show-implicit"   -> parseArgs opts{ optImplicit = True } acc rest
    "--show-irrelevant" -> parseArgs opts{ optIrrelevant = True } acc rest
    "--pattern-lambda"  -> parseArgs opts{ optPmLambda = True } acc rest
    "--no-timeout"      -> parseArgs opts{ optNoTimeout = True } acc rest
    "-t"                -> withArg rest $ \ v r ->
                             parseArgs opts{ optTimeout = readMaybeDouble v } acc r
    "--timeout"         -> withArg rest $ \ v r ->
                             parseArgs opts{ optTimeout = readMaybeDouble v } acc r
    "-n"                -> withArg rest $ \ v r ->
                             parseArgs opts{ optNorm = Just v } acc r
    "--normalise"       -> withArg rest $ \ v r ->
                             parseArgs opts{ optNorm = Just v } acc r
    "--normalize"       -> withArg rest $ \ v r ->
                             parseArgs opts{ optNorm = Just v } acc r
    "--mode"            -> withArg rest $ \ v r ->
                             parseArgs opts{ optMode = Just v } acc r
    "--root"            -> withArg rest $ \ v r ->
                             parseArgs opts{ optRoot = Just v } acc r
    "--expect-revision" -> withArg rest $ \ v r ->
                             parseArgs opts{ optRevision = readMaybeInt v } acc r
    _ | "--" `isPrefixOf` a, not passThrough -> die' $ "unknown flag: " ++ a
      | otherwise -> parseArgs opts (a : acc) rest
    where
    withArg [] _      = die' $ a ++ " needs an argument"
    withArg (v:r) k   = k v r
    -- Everything after @load FILE@ is Agda's, so an unknown flag there
    -- is an argument for it rather than a mistake of ours.
    passThrough = case reverse acc of
      "load" : _ : _ -> True
      _              -> False

-- | Turn a verb and its arguments into a request object.
buildRequest :: Options -> FilePath -> String -> [String] -> IO Value
buildRequest opts root verb args = do
  (method, params, defTimeout) <- case (verb, args) of
    ("load", file : rest) -> do
      abs' <- canonicalizePath file `catch` \ (_ :: IOException) ->
                return (root </> file)
      return ("load", [ "file" .= abs', "args" .= rest ] ++ profile, 600)
    ("load", []) -> die' "load needs a file"

    ("reload", []) -> return ("reload", profile, 600)

    ("goals", []) -> return ("goals", norm, 120)

    ("goal", [g])    -> return ("goal", ("goal" .= goalRef g) : norm, 120)
    ("context", [g]) -> return ("context", ("goal" .= goalRef g) : norm, 120)

    ("check", [g, e]) ->
      return ("check", [ "goal" .= goalRef g, "expr" .= e ] ++ norm, 120)

    ("infer", [e])     -> return ("infer", ("expr" .= e) : norm, 120)
    ("infer", [g, e])  ->
      return ("infer", [ "goal" .= goalRef g, "expr" .= e ] ++ norm, 120)

    (v, [e]) | v `elem` normVerbs -> return ("normalise", ("expr" .= e) : mode, 120)
    (v, [g, e]) | v `elem` normVerbs ->
      return ("normalise", [ "goal" .= goalRef g, "expr" .= e ] ++ mode, 120)

    ("give", [g, e]) -> return
      ("give", [ "goal" .= goalRef g, "expr" .= e, "force" .= optForce opts ], 600)
    ("elaborate", [g, e]) -> return
      ("elaborate", [ "goal" .= goalRef g, "expr" .= e ] ++ norm, 600)
    ("refine", [g]) -> return
      ("refine", [ "goal" .= goalRef g ] ++ pmLambda, 600)
    ("refine", [g, e]) -> return
      ("refine", [ "goal" .= goalRef g, "expr" .= e ] ++ pmLambda, 600)
    ("intro", [g]) -> return
      ("intro", [ "goal" .= goalRef g ] ++ pmLambda, 600)

    ("case", [g])    -> return ("case", [ "goal" .= goalRef g ], 600)
    ("case", [g, v]) -> return
      ("case", [ "goal" .= goalRef g, "vars" .= v ], 600)

    ("solve", [])  -> return ("solve", norm, 600)
    ("solve", [g]) -> return ("solve", ("goal" .= goalRef g) : norm, 600)

    ("auto", [])  -> return ("auto", norm, 600)
    ("auto", [a]) | isGoalRef a -> return ("auto", ("goal" .= goalRef a) : norm, 600)
                  | otherwise   -> return ("auto", ("args" .= a) : norm, 600)
    ("auto", [g, a]) -> return
      ("auto", [ "goal" .= goalRef g, "args" .= a ] ++ norm, 600)

    ("check-candidates", g : es@(_ : _)) -> return
      ("checkCandidates", [ "goal" .= goalRef g, "exprs" .= es ] ++ norm, 300)
    ("candidates", g : es@(_ : _)) -> return
      ("checkCandidates", [ "goal" .= goalRef g, "exprs" .= es ] ++ norm, 300)

    ("helper", [g, e]) -> return
      ("helperType", [ "goal" .= goalRef g, "expr" .= e ] ++ norm, 120)

    ("search", es@(_ : _)) ->
      return ("search", ("names" .= unwords es) : norm, 120)

    -- `find PATTERN in MODULE` is the design's own notation for the
    -- scoped query; --in says the same thing where a script would
    -- rather pass a flag.
    ("find", pat : rest) -> do
      scope <- case rest of
        []             -> return $ optIn opts
        ["in", m]      -> return $ Just m
        [m] | Nothing <- optIn opts -> return $ Just m
        _              -> die' "find takes a pattern and an optional `in MODULE`"
      return ("findNames", concat
        [ [ "pattern" .= pat ]
        , [ "module" .= m | Just m <- [scope] ]
        , [ "glob"   .= True | optGlob opts ]
        , [ "kinds"  .= optKinds opts | not (Prelude.null (optKinds opts)) ]
        , [ "type"   .= t | Just t <- [optType opts] ]
        , [ "limit"  .= n | Just n <- [optLimit opts] ]
        ], 120)
    ("find", []) -> die' "find needs a pattern"

    -- `index` refreshes what has moved; --force re-renders everything.
    -- --in scopes the report it prints, not the build.
    ("index", []) ->
      return ("buildIndex", concat
        [ [ "force"  .= optForce opts ]
        , [ "module" .= m | Just m <- [optIn opts] ]
        ], 1800)
    ("index", ["in", m]) ->
      return ("buildIndex", [ "force" .= optForce opts, "module" .= m ], 1800)

    (v, [m]) | v `elem` moduleVerbs ->
      return ("moduleContents", ("module" .= m) : norm, 120)
    (v, [g, m]) | v `elem` moduleVerbs ->
      return ("moduleContents", [ "goal" .= goalRef g, "module" .= m ] ++ norm, 120)

    (v, [n]) | v `elem` whyVerbs ->
      return ("whyInScope", [ "name" .= n ], 120)
    (v, [g, n]) | v `elem` whyVerbs, isGoalRef g ->
      return ("whyInScope", [ "goal" .= goalRef g, "name" .= n ], 120)

    (v, [pos]) | v `elem` defVerbs -> do
      unless (':' `elem` pos) $ die' $ v ++ " takes a LINE:COL position"
      return ("definitionAt", [ "position" .= goalRef pos ], 120)

    ("constraints", []) -> return ("constraints", norm, 120)
    ("status", [])      -> return ("status", [], 0)
    ("handshake", [])   -> return ("handshake", [], 0)
    ("abort", [])       -> return ("abort", [], 0)
    ("stop", [])        -> return ("stop", [], 0)

    _ -> die' $ "bad arguments for " ++ show verb ++ "; see --help"

  when (not (null (optProfile opts)) && method `notElem` ["load", "reload" :: Text]) $
    die' "--profile applies to load and reload"
  let secs = case (optNoTimeout opts, optTimeout opts) of
        (True, _)       -> 0
        (_, Just t)     -> t
        (_, Nothing)    -> defTimeout
  return $ object $
    [ "method" .= (method :: Text), "params" .= object params ] ++
    [ "timeoutMs" .= (round (secs * 1000) :: Int) | secs > 0 ] ++
    [ "expectRevision" .= r | Just r <- [optRevision opts] ] ++
    [ "showImplicit" .= True | optImplicit opts ] ++
    [ "showIrrelevant" .= True | optIrrelevant opts ]
  where
  normVerbs = ["normalise", "normalize", "norm"]
  moduleVerbs = ["module-contents", "module"]
  whyVerbs = ["why", "why-in-scope"]
  defVerbs = ["definition", "def"]
  norm = [ "normalise" .= n | Just n <- [optNorm opts] ]
  profile = [ "profile" .= optProfile opts | not (null (optProfile opts)) ]
  mode = [ "mode" .= m | Just m <- [optMode opts] ]
  pmLambda = [ "patternLambda" .= True | optPmLambda opts ]

-- | Does this argument name a goal?  Used where a goal is optional and
--   another string could take its place.
isGoalRef :: String -> Bool
isGoalRef s = case break (== ':') s of
  (l, ':' : c) -> digits l && digits c
  _            -> digits s
  where digits x = not (null x) && all isDigit x

-- | A goal is addressed by its interaction id, or by a @LINE:COL@
--   position inside it (which survives renumbering).
goalRef :: String -> Value
goalRef s = case break (== ':') s of
  (l, ':' : c) | all isDigit l, all isDigit c, not (null l), not (null c) ->
    object [ "line" .= (read l :: Int), "col" .= (read c :: Int) ]
  _ | all isDigit s, not (null s) -> toJSON (read s :: Int)
    | otherwise -> toJSON s

-- | Send the request, print the answer.
runRequest :: Options -> FilePath -> FilePath -> String -> Value -> IO ()
runRequest opts root sock verb request = do
  h <- connectDaemon root sock
  BS.hPutStrLn h (encode request)
  hFlush h
  code <- relay h
  hClose h `catch` \ (_ :: IOException) -> return ()
  exitWith code
  where
  relay h = do
    line <- (Just . BS.fromStrict <$> B.hGetLine h)
              `catch` \ (_ :: IOException) -> return Nothing
    case line of
      Nothing -> die' $ "the daemon closed the connection (see " ++ sock ++ ".log)"
      Just l  -> case decode l :: Maybe Value of
        Nothing -> die' $ "unreadable answer: " ++ BS.unpack l
        Just v
          | Just r <- field "result" v -> do
              report v r
              return ExitSuccess
          | Just e <- field "error" v -> do
              reportError v e
              return $ ExitFailure 1
          | otherwise -> do
              -- A progress event: keep it out of the way of the answer.
              if optJson opts
                then BS.putStrLn l
                else forM_ (field "message" v) $ \ m ->
                       hPutStrLn stderr $ "… " ++ asText m
              relay h

  report v r
    | optJson opts = BS.putStrLn (encode v)
    | otherwise = do
        when (field "resynced" r == Just (Bool True)) $
          hPutStrLn stderr "[file changed on disk: reloaded]"
        case field "text" r of
          Just t  -> putStrLn (asText t)
          Nothing -> BS.putStrLn (encode r)

  reportError v e
    | optJson opts = BS.putStrLn (encode v)
    | otherwise = do
        let code = maybe "error" asText (field "code" e)
            msg  = maybe "" asText (field "message" e)
        hPutStrLn stderr $ verb ++ ": " ++ code
        hPutStrLn stderr msg
        -- The timings a timeout carries are a table, not a line of
        -- JSON, and the message already names the definition.
        let profile = field "profile" =<< field "data" e
            rest    = case field "data" e of
              Just (Object d) ->
                Object (KeyMap.delete "profile" (KeyMap.delete "checking" d))
              d               -> fromMaybe Null d
        forM_ profile $ \ p -> hPutStr stderr $ slowest p
        case rest of
          Object d | KeyMap.null d -> return ()
          Null                     -> return ()
          d -> BS.hPutStrLn stderr $ "data: " <> encode d

  -- The most expensive accounts of a profile, as a short table.
  slowest p = case [ (asText a, ms)
                   | Just (Array rows) <- [field "accounts" p]
                   , r <- foldr (:) [] rows
                   , Just a <- [field "account" r]
                   , Just (Number ms) <- [field "ms" r] ] of
    []   -> ""
    rows -> unlines $ "Slowest so far (CPU time):" :
      [ "  " ++ pad 8 (show (round ms :: Integer)) ++ " ms  " ++ a
      | (a, ms) <- take 10 rows ]
  pad n s = replicate (n - length s) ' ' ++ s

field :: Text -> Value -> Maybe Value
field k (Object o) = KeyMap.lookup (Key.fromText k) o
field _ _          = Nothing

asText :: Value -> String
asText (String t) = T.unpack t
asText v          = BS.unpack (encode v)

-- | Attach to the daemon, starting it if necessary.
connectDaemon :: FilePath -> FilePath -> IO Handle
connectDaemon root sock = attempt >>= \case
  Just h  -> return h
  Nothing -> do
    spawnDaemon root sock
    waitFor (600 :: Int)  -- up to a minute; Agda's startup is not instant
  where
  attempt = do
    r <- try $ do
      s <- socket AF_UNIX Stream defaultProtocol
      connect s (SockAddrUnix sock) `onException` close s
      socketToHandle s ReadWriteMode
    case r of
      Right h -> Just h <$ hSetBinaryMode h True
      Left (_ :: IOException) -> return Nothing

  waitFor 0 = die' $ "could not start the Agda daemon (see " ++ sock ++ ".log)"
  waitFor n = do
    threadDelay 100000
    attempt >>= maybe (waitFor (n - 1)) return

spawnDaemon :: FilePath -> FilePath -> IO ()
spawnDaemon root sock = do
  self <- getExecutablePath
  -- One handle for both streams: GHC's file locking rules out opening
  -- the log twice, and 'createProcess' duplicates the descriptor.
  log' <- openFile (sock ++ ".log") AppendMode
  void $ createProcess (proc self ["--daemon", root, sock])
    { std_in      = NoStream
    , std_out     = UseHandle log'
    , std_err     = UseHandle log'
    , new_session = True
    , cwd         = Just root
    }

------------------------------------------------------------------------
-- * The daemon

data Session = Session
  { sesStdin  :: Handle
  , sesLock   :: MVar ()                            -- ^ Serialises writes to Agda.
  , sesRoutes :: MVar (Map Integer (TChan (Maybe BS.ByteString)))
  , sesNextId :: MVar Integer
  , sesBusy   :: TVar Int                           -- ^ Requests in flight.
  , sesDone   :: MVar ()                            -- ^ Filled when the daemon should exit.
  , sesLast   :: TVar [B.ByteString]                -- ^ Agda's last words on stderr, newest first.
  , sesQuiet  :: MVar ()                            -- ^ Filled when Agda's stderr has closed.
  }

-- | How many lines of Agda's stderr to keep.  An internal error is
--   four or five, a call stack a few more.
lastWordsKept :: Int
lastWordsKept = 30

daemon :: FilePath -> FilePath -> IO ()
daemon root sock = do
  bin  <- agdaBin
  args <- agdaArgs
  listener <- bindSocket
  case listener of
    Nothing -> hPutStrLn stderr "another daemon is already listening; exiting"
    Just server -> do
      -- Agda's stderr is read rather than inherited: when the process
      -- dies it is the only account of why, and the client is owed it.
      (Just hin, Just hout, Just herr, ph) <-
        createProcess (proc bin ("--interaction-agent" : args))
          { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
          , cwd = Just root }
      hSetBinaryMode hin  True
      hSetBinaryMode hout True
      hSetBinaryMode herr True
      hSetBuffering  hin  LineBuffering
      session <- Session hin <$> newMVar () <*> newMVar Map.empty <*> newMVar 0
                             <*> newTVarIO 0 <*> newEmptyMVar
                             <*> newTVarIO [] <*> newEmptyMVar
      _ <- forkIO $ keepLastWords session herr
      _ <- forkIO $ router session hout
      idle <- idleTimeout
      serve server session idle
        `finally` do
          removeFile sock `catch` \ (_ :: IOException) -> return ()
          terminateProcess ph
  where
  bindSocket = do
    -- A socket file left behind by a crashed daemon is not a live
    -- daemon: drop it and take over.
    stale <- doesPathExist sock
    when stale $ do
      live <- probe
      if live
        then return ()
        else removeFile sock `catch` \ (_ :: IOException) -> return ()
    still <- doesPathExist sock
    if still then return Nothing else do
      s <- socket AF_UNIX Stream defaultProtocol
      r <- try $ do
        bind s (SockAddrUnix sock)
        listen s 16
      case r of
        Left (_ :: IOException) -> Nothing <$ close s
        Right ()                -> return (Just s)

  probe = do
    r <- try $ do
      s <- socket AF_UNIX Stream defaultProtocol
      connect s (SockAddrUnix sock) `finally` close s
    return $ case r of
      Right () -> True
      Left (_ :: IOException) -> False

-- | Read Agda's answers and hand each line to the client that asked.
--
--   When Agda goes away the daemon has nothing left to serve, so it
--   asks the main thread to shut down: an exception here would end
--   this thread only, leaving a daemon that owns a socket and answers
--   nothing.
router :: Session -> Handle -> IO ()
router session hout = loop `catch` \ (_ :: IOException) -> shutdown
  where
  loop = do
    eof <- hIsEOF hout
    if eof then shutdown else do
      line <- BS.fromStrict <$> B.hGetLine hout
      case decode line :: Maybe Value of
        Nothing -> BS.hPutStrLn stderr line  -- not ours: log it
        Just v -> case field "id" v of
          Just (Number n) -> do
            let i = truncate n
            routes <- readMVar (sesRoutes session)
            forM_ (Map.lookup i routes) $ \ chan -> do
              atomically $ writeTChan chan (Just line)
              when (terminal v) $ do
                atomically $ writeTChan chan Nothing
                modifyMVar_ (sesRoutes session) $ return . Map.delete i
          _ -> BS.hPutStrLn stderr line  -- the ready banner, or stray output
      loop

  terminal v = any (\ k -> field k v /= Nothing) ["result", "error"]

  shutdown = do
    -- Whatever Agda printed on the way out belongs in the answer: an
    -- internal error is reported there and nowhere else, and without it
    -- the client is left with a dead session and no location, no name
    -- and no error class.  stdout and stderr close at their own pace,
    -- so give the reader a moment to catch up.
    _ <- timeout 500000 $ readMVar (sesQuiet session)
    said <- reverse <$> readTVarIO (sesLast session)
    let txt = T.strip $ T.decodeUtf8With T.lenientDecode $ B.unlines said
        msg | T.null txt = base
            | otherwise  = base <> ", saying:\n" <> txt
        base = "the Agda process exited" :: Text
    routes <- readMVar (sesRoutes session)
    forM_ (Map.elems routes) $ \ chan -> atomically $ do
      writeTChan chan $ Just $ encode $ object
        [ "error" .= object
            [ "code" .= ("internalError" :: Text)
            , "message" .= msg ] ]
      writeTChan chan Nothing
    void $ tryPutMVar (sesDone session) ()

-- | Read Agda's stderr, keeping the last lines of it and passing all of
--   them on to the daemon's own log.
keepLastWords :: Session -> Handle -> IO ()
keepLastWords session herr = loop `catch` \ (_ :: IOException) -> quiet
  where
  loop = do
    eof <- hIsEOF herr
    if eof then quiet else do
      line <- B.hGetLine herr
      B.hPutStrLn stderr line
      atomically $ modifyTVar' (sesLast session) $ take lastWordsKept . (line :)
      loop

  quiet = void $ tryPutMVar (sesQuiet session) ()

-- | Accept connections until the idle timeout expires, a client says
--   @stop@, or Agda exits.
--
--   Each connection is served on its own thread.  Agda executes the
--   requests one after another anyway, but @abort@ is acted upon as
--   soon as it is /read/, so it has to be able to arrive while another
--   request is in flight -- which it cannot if the daemon is busy
--   relaying that request's answer.
serve :: Socket -> Session -> Int -> IO ()
serve server session idle = do
  acceptor <- forkIO loop
  takeMVar (sesDone session)
  killThread acceptor
  where
  loop = timeout (idle * 1000000) (accept server) >>= \case
    Nothing -> do
      -- Idle, but do not walk out on a request that is still running.
      busy <- readTVarIO (sesBusy session)
      if busy > 0 then loop else quit
    Just (conn, _) -> do
      h <- socketToHandle conn ReadWriteMode
      hSetBinaryMode h True
      _ <- forkIO $ do
        stop <- handle' h `catch` \ (_ :: IOException) -> return False
        hClose h `catch` \ (_ :: IOException) -> return ()
        when stop quit
      loop

  quit = void $ tryPutMVar (sesDone session) ()

  handle' h = do
    line <- BS.fromStrict <$> B.hGetLine h
    case decode line :: Maybe Value of
      Nothing -> False <$ answer h (protocolError "the request is not JSON")
      Just v@(Object o)
        | field "method" v == Just (String "stop") -> do
            answer h $ object
              [ "result" .= object
                  [ "stopped" .= True, "text" .= ("daemon stopped" :: Text) ] ]
            return True
        | otherwise -> do
            i <- modifyMVar (sesNextId session) $ \ n -> return (n + 1, n + 1)
            chan <- newTChanIO
            modifyMVar_ (sesRoutes session) $ return . Map.insert i chan
            let request = Object $ KeyMap.insert (Key.fromText "id") (toJSON i) o
            bracket_ (bump 1) (bump (-1)) $ do
              withMVar (sesLock session) $ \ () -> do
                BS.hPutStrLn (sesStdin session) (encode request)
                hFlush (sesStdin session)
              pump h chan
            return False
      Just _ -> False <$ answer h (protocolError "the request is not an object")

  pump h chan = atomically (readTChan chan) >>= \case
    Nothing -> return ()
    Just l  -> do
      BS.hPutStrLn h l
      hFlush h
      pump h chan

  answer h v = BS.hPutStrLn h (encode v) >> hFlush h

  protocolError msg = object
    [ "error" .= object
        [ "code" .= ("invalidRequest" :: Text), "message" .= (msg :: Text) ] ]

  bump n = atomically $ modifyTVar' (sesBusy session) (+ n)

------------------------------------------------------------------------
-- * Odds and ends

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

readMaybeDouble :: String -> Maybe Double
readMaybeDouble s = case reads s of
  [(n, "")] -> Just n
  _         -> case reads s :: [(Int, String)] of
    [(n, "")] -> Just (fromIntegral n)
    _         -> Nothing

die' :: String -> IO a
die' msg = hPutStrLn stderr ("agda-client: " ++ msg) >> exitWith (ExitFailure 2)

usage :: String
usage = unlines
  [ "agda-client — drive an Agda proof session from the command line."
  , ""
  , "Usage: agda-client [OPTIONS] VERB [ARGS...]"
  , ""
  , "Queries (these never write to your files):"
  , "  load FILE [AGDA-ARG...]  type-check FILE and list its goals"
  , "  reload                   type-check the session's file again"
  , "  goals                    list the goals with their types and ranges"
  , "  goal GOAL                goal type, context, boundary and constraints"
  , "  context GOAL             just the context of GOAL"
  , "  check GOAL EXPR          type-check EXPR against GOAL without filling it"
  , "  infer [GOAL] EXPR        infer the type of EXPR"
  , "  normalise [GOAL] EXPR    normalise EXPR (aliases: normalize, norm)"
  , "  constraints              list the unsolved constraints"
  , "  candidates GOAL EXPR...  check several candidates against GOAL at once"
  , "  helper GOAL EXPR         the type a helper function 'f args' would need"
  , "  search TERM...           names in scope whose type mentions all TERMs"
  , "  find PATTERN [in MOD]    names matching PATTERN anywhere the project can"
  , "                           import from, ranked, with their types"
  , "  module-contents [GOAL] M the names and modules M exports"
  , "  why [GOAL] NAME          why NAME is in scope, and what it can mean"
  , "  definition LINE:COL      where the name at LINE:COL is defined"
  , ""
  , "Proof steps (these rewrite the file, reload it, and list the new goals):"
  , "  give GOAL EXPR           fill GOAL with EXPR"
  , "  elaborate GOAL EXPR      fill GOAL with the elaborated form of EXPR"
  , "  refine GOAL [EXPR]       refine GOAL by EXPR, leaving new goals for its"
  , "                           arguments; with no EXPR, introduce"
  , "  intro GOAL               introduce the constructor or lambda GOAL asks for"
  , "  case GOAL [VARS]         split GOAL's clause on the space separated VARS;"
  , "                           with no VARS, split on the result"
  , "  solve [GOAL]             write out goals the type checker has already"
  , "                           solved, all of them or just GOAL"
  , "  auto [GOAL] [ARGS]       proof search (Mimer); ARGS is passed to it, e.g."
  , "                           '-t 5s' or '-l' to list candidates instead"
  , ""
  , "Index (find reads a name index built beside the interface files):"
  , "  index [in MOD]           re-index what has moved since the last build;"
  , "                           with --force, build the whole index again"
  , ""
  , "Session:"
  , "  status                   what the session has loaded"
  , "  handshake                protocol and Agda version, methods, defaults"
  , "  abort                    interrupt the command the daemon is running"
  , "  stop                     shut the daemon down"
  , ""
  , "GOAL is an interaction id (0, 1, ...) or a LINE:COL position inside a"
  , "goal, which survives the renumbering that follows an edit."
  , ""
  , "Options:"
  , "  --json                   print the raw protocol answer"
  , "  --profile[=OPTION]       load, reload: check the file from source with"
  , "                           Agda's --profile=OPTION (default: definitions),"
  , "                           for this load only, and print the CPU time per"
  , "                           definition; repeatable"
  , "  --force                  give: skip the safety checks;"
  , "                           index: re-render the whole index, not just"
  , "                           the modules that have moved"
  , "  --in MODULE              find: restrict to this top-level module prefix;"
  , "                           index: scope the coverage report it prints"
  , "  --glob                   find: read * and ? in the pattern as wildcards"
  , "                           (off by default: Agda names contain *)"
  , "  --kind KIND              find: restrict to this kind of name, repeatable"
  , "                           (FunName, DataName, RecName, ConName, FldName,"
  , "                           AxiomName, PrimName, MacroName, PatternSynName,"
  , "                           GeneralizeName; the Name suffix is optional)"
  , "  --type NAMES             find: only names whose type mentions all of"
  , "                           these space separated NAMES"
  , "  --limit N                find: print at most N hits (default 50)"
  , "  -p, --pattern-lambda     refine/intro: introduce a pattern matching lambda"
  , "  -n, --normalise MODE     asIs | instantiated | headNormal | simplified"
  , "                           | normalised"
  , "  --mode MODE              normalise: default | head | ignoreAbstract | show"
  , "  --show-implicit          print implicit arguments"
  , "  --show-irrelevant        print irrelevant arguments"
  , "  -t, --timeout SECS       abort the request after SECS (default: 120,"
  , "                           600 for load, reload and the proof steps)"
  , "  --no-timeout             let the request run forever"
  , "  --expect-revision N      fail instead of reloading if the file changed"
  , "                           on disk since revision N"
  , "  --root DIR               use DIR as the project root"
  , "  --version                print agda-client's version"
  , ""
  , "load passes AGDA-ARG... on to Agda, and remembers them for the reloads"
  , "that follow: `load FILE --without-K` type-checks FILE with that flag."
  , "A `--` ends agda-client's own options, so a flag it shares with Agda can"
  , "be aimed at Agda instead: `load FILE -- --show-implicit`."
  , ""
  , "A request that times out names the definition it was checking and lists"
  , "the slowest definitions so far; `load --profile FILE` measures them all."
  , "Exit status: 0 for an answer (a rejected `check` is an answer), 1 for a"
  , "failed request (an Agda error, a timeout), 2 when the client itself"
  , "fails (bad arguments, no daemon)."
  , ""
  , "find matches names, search matches types: find looks for a spelling in"
  , "every module the project could import, search for the names in scope whose"
  , "type mentions given identifiers.  find ranks its hits: 1 the whole name,"
  , "2 a prefix of it, 3 a whole hyphen- or underscore-separated component,"
  , "4 a substring of it, and — only with --type — 5 a name its type mentions."
  , "It answers from an index, so it finds nothing until `index` has built one,"
  , "and every answer says how many modules are covered, stale or unchecked."
  , "`index` again re-renders only the modules whose interfaces have moved, so"
  , "keeping it current after an edit costs a fraction of the first build."
  , ""
  , "The file on disk is the source of truth: edit it with your own tools and"
  , "the next query reloads it automatically.  A proof step rewrites the file"
  , "itself and reloads it, so the goals it prints are the ones of the new"
  , "revision; goals are renumbered by that reload."
  , ""
  , "Environment: AGDA_CLIENT_BIN (default: agda), AGDA_CLIENT_ARGS,"
  , "AGDA_CLIENT_ROOT, AGDA_CLIENT_IDLE (daemon idle seconds, default 14400)."
  ]
