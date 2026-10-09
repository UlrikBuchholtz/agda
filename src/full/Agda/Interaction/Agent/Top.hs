{-# OPTIONS_GHC -Wunused-imports #-}

-- | The agent interaction frontend: @agda --interaction-agent@.
--
--   A JSON-lines request\/response frontend meant to be driven by a
--   program (typically the @agda-client@ client on behalf of a coding
--   agent) rather than by an editor.  See
--   "Agda.Interaction.Agent.Protocol" for the wire format and
--   @notes\/agent-interface.md@ for the design.
--
--   Differences to the Emacs and JSON frontends:
--
--   * Requests carry an @id@ and produce exactly one terminal line, so
--     no prompt sniffing or response aggregation is needed.
--
--   * The session owns the current revision of the loaded file: a query
--     against a file that changed on disk transparently reloads it
--     first and reports @resynced@, instead of failing.
--
--   * Any request may carry a @timeoutMs@; on expiry the existing abort
--     machinery is triggered and the session stays usable.

module Agda.Interaction.Agent.Top
  ( agentREPL
  ) where

import Prelude hiding ( null )

import Control.Concurrent          ( forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception           ( IOException, catch, finally, throwIO, try )
import Control.Monad               ( forM, unless, void, when )
import Control.Monad.Except        ( ExceptT, MonadError(..), runExceptT )
import Control.Monad.IO.Class      ( MonadIO(..) )
import Control.Monad.State         ( evalStateT, gets, runStateT )
import Control.Monad.STM           ( atomically, retry )
import Control.Monad.Trans         ( lift )

import Data.Aeson hiding ( Error )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types            ( Pair, Parser, parseEither )
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char                   ( isSpace, toLower )
import Data.IORef
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe                  ( fromMaybe, isJust, isNothing, listToMaybe )
import Data.Ord                    ( Down(..) )
import Data.Text                   ( Text )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word                   ( Word32 )

import System.Directory            ( getCurrentDirectory, removeFile )
import System.Environment          ( getArgs, getExecutablePath )
import System.Exit                 ( ExitCode(..) )
import System.FilePath             ( takeDirectory )
import System.IO
import System.Process
  ( CreateProcess(..), StdStream(..), proc, terminateProcess
  , waitForProcess, withCreateProcess )
import System.IO.Error             ( isDoesNotExistError )
import System.Timeout              ( timeout )

import Agda.Benchmarking qualified as Bench
import Agda.Interaction.Agent.Diagnostic
import Agda.Interaction.Agent.Edit
import Agda.Interaction.NameIndex
  ( Coverage(..), NameQuery(..), QueryResult(..)
  , coverageStale, coverageText, coverageValue, hitsText, hitsValue
  , indexErrorText, nameIndexProjectRoot, queryIndex, scanCoverageIn )
import Agda.Interaction.Agent.Protocol
import Agda.Interaction.Base
import Agda.Interaction.BasicOps qualified as B
import Agda.Interaction.Command    ( CommandM )
import Agda.Interaction.FindFile   ( toIFile )
import Agda.Interaction.EmacsTop
  ( explainWhyInScope, namedMetaOf, prettyGoalType, prettyGoals
  , prettyInferredType, prettyModuleContents, prettyNamedType
  , prettyNormalForm, prettyResponseContext, prettySearchAbout
  , prettyTypeOfMeta )
import Agda.Interaction.Highlighting.Precise
  ( Aspects(..), DefinitionSite(..), HighlightingInfo, IsBasicRangeMap(..) )
import Agda.Interaction.InteractionTop
import Agda.Interaction.Options
  ( lensOptShowImplicit, lensOptShowIrrelevant, optAbsoluteIncludePaths
  , optProfiling )
import Agda.Interaction.Options.ProfileOptions
  ( ProfileOption(..), containsProfileOption )
import Agda.Interaction.Output     ( OutputConstraint, OutputForm )
import Agda.Interaction.Response   as R

import Agda.Syntax.Abstract qualified as A
import Agda.Syntax.Abstract.Name    ( amodName, anameKind, anameName )
import Agda.Syntax.Abstract.Pretty ( prettyATop )
import Agda.Syntax.Common          ( InteractionId(..) )
import Agda.Syntax.Common.Aspect qualified as Asp
import Agda.Syntax.Common.Pretty
  ( Doc, pretty, prettyShow, render, text, vcat )
import Agda.Syntax.Concrete qualified as C
import Agda.Syntax.Internal        ( alwaysUnblock )
import Agda.Syntax.Position
import Agda.Syntax.Scope.Base      ( WhyInScopeData(..) )

import Agda.TypeChecking.Errors    ( prettyError )
import Agda.TypeChecking.Monad hiding ( (.=), InternalError )
import Agda.TypeChecking.Pretty    ( prettyTCM )
import Agda.TypeChecking.Pretty.Warning ( filterTCWarnings )

import Agda.Utils.Benchmark qualified as Benchmark
import Agda.Utils.DocTree qualified as DocTree
import Agda.Utils.FileName         ( absolute, filePath )
import Agda.Utils.Hash             ( Hash, hashTextFile )
import Agda.Utils.Maybe.Strict qualified as Strict
import Agda.Utils.Null
import Agda.Utils.Range qualified as HRange
import Agda.Utils.Time             ( CPUTime(..), getCPUTime )
import Agda.Utils.Trie qualified as Trie
import Agda.Utils.WithDefault      ( lensKeepDefault )

import Agda.VersionCommit          ( versionWithCommitInfo )

------------------------------------------------------------------------
-- * Session state

-- | What the agent frontend remembers between requests.
--
--   The 'CommandState' is reset by 'maybeAbort' after an abort, so the
--   file we are working on is tracked here as well; the next request
--   then reloads it (see 'resync').
data AgentSession = AgentSession
  { sesFile     :: Maybe AbsolutePath
      -- ^ The file the session is about, if 'load' has succeeded once.
  , sesArgs     :: [String]
      -- ^ The Agda arguments the file was loaded with.
  , sesHash     :: Maybe Hash
      -- ^ Hash of the file contents as loaded.
  , sesRevision :: !Int
      -- ^ Incremented by every (re)load.
  , sesChecked  :: !Bool
      -- ^ Did the last load run to completion without an error?
      --   Unsolved goals and warnings do not make it 'False'.
  , sesFailure  :: Maybe Diagnostic
      -- ^ The error the last load stopped at, if it stopped at one.
      --
      --   A failed load leaves no current file, so without this every
      --   query that followed would check the file again and fail the
      --   same way -- a full re-check per question on a large module.
      --   While the file on disk is still the one that failed, a query
      --   is answered with this error instead; the first edit clears
      --   it, and so do 'load' and 'reload'.
  , sesDirty    :: !Bool
      -- ^ Has a command changed the proof state since the last load?
      --
      --   A mutation reloads when it is done, so this is only ever
      --   'True' where one gave up in the middle: a @solve@ whose
      --   second give failed has consumed a goal that the file on disk
      --   still has.  The next request reloads rather than answering
      --   from a state that no longer matches the source.
  }

initialSession :: AgentSession
initialSession = AgentSession Nothing [] Nothing 0 False Nothing False

-- | Read-only plumbing shared by the request loop and the output callback.
data AgentEnv = AgentEnv
  { envSession   :: IORef AgentSession
  , envResponses :: IORef [Response]
      -- ^ Responses of the command in flight, most recent first.
  , envRequests  :: TChan Incoming
      -- ^ Filled by the stdin reader thread.
  , envCounter   :: TVar Integer
      -- ^ Number of the last command handed to the 'CommandQueue'.
  , envRunning   :: TVar Bool
      -- ^ Is that command, or the index builder, still running?  Set
      --   together with 'envCounter', so that an @abort@ read from
      --   stdin can tell whether it has interrupted anything.
  , envCurrentId :: IORef Value
      -- ^ Id of the request in flight, for progress events.
  , envOutLock   :: MVar ()
  }

-- | A line read from stdin.
data Incoming
  = Incoming AgentRequest Bool
      -- ^ The 'Bool' says, for an @abort@, whether a command was
      --   running when it was read, and so whether it aborted one.
  | Malformed String
  | EndOfInput

------------------------------------------------------------------------
-- * The frontend

-- | Run the agent frontend: read JSON-lines requests from stdin, write
--   JSON-lines events and results to stdout.
agentREPL :: TCM () -> TCM ()
agentREPL setup = do
  liftIO $ do
    hSetBuffering stdin  LineBuffering
    hSetEncoding  stdin  utf8
    hSetEncoding  stderr utf8
    -- We write pre-encoded UTF-8 'ByteString's, so stdout must not
    -- re-encode them.
    hSetBinaryMode stdout True
    hSetBuffering  stdout LineBuffering

  queue <- liftIO $ CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  env   <- liftIO $ AgentEnv
    <$> newIORef initialSession
    <*> newIORef []
    <*> newTChanIO
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newIORef Null
    <*> newMVar ()

  setInteractionOutputCallback (agentCallback env)
  _ <- liftIO $ forkIO $ readRequests env queue

  liftIO $ emit env $ object
    [ "event"     .= ("ready" :: Text)
    , "handshake" .= handshakeValue
    ]

  handleCommand_ (lift setup) `evalStateT` initCommandState queue

  opts <- commandLineOptions
  _ <- mainLoop env `runStateT`
         (initCommandState queue)
           { optionsOnReload = opts{ optAbsoluteIncludePaths = [] } }
  return ()

-- | Collect responses of the running command; forward progress
--   messages to the client right away.
agentCallback :: AgentEnv -> InteractionOutputCallback
agentCallback env r = liftIO $ do
  case r of
    Resp_RunningInfo n doc | n <= 1 -> do
      i <- readIORef (envCurrentId env)
      emit env $ eventLine i "progress"
        [ "message" .= DocTree.treeToTextNoAnn doc ]
    _ -> return ()
  modifyIORef' (envResponses env) (r :)

-- | Write one JSON line.
emit :: AgentEnv -> Value -> IO ()
emit env v = withMVar (envOutLock env) $ \ () -> do
  BS.hPut stdout $ encode v
  BS.hPut stdout "\n"
  hFlush stdout

-- | Read request lines from stdin.
--
--   @abort@ is acted upon here rather than in the main loop, since the
--   main loop is busy running the command that is to be aborted.
readRequests :: AgentEnv -> CommandQueue -> IO ()
readRequests env queue = loop `catch` \ e -> do
    -- Nothing more will arrive: the stream is closed, or a line does
    -- not decode.  Say so and end the session.  Left to itself this
    -- thread would die quietly and the main loop would wait for a
    -- request that can never come.
    post $ Malformed $
      "reading from standard input failed: " ++ show (e :: IOException)
    post EndOfInput
  where
  loop = do
    eof <- isEOF
    if eof then post EndOfInput else do
      line <- getLine
      case dropWhile (== ' ') line of
        ""        -> loop
        '-':'-':_ -> loop
        _         -> do
          case eitherDecodeStrict' (T.encodeUtf8 (T.pack line)) of
            Left err  -> post $ Malformed err
            Right req -> do
              hit <- if reqMethod req /= "abort" then return False else
                atomically $ do
                  n <- readTVar (envCounter env)
                  modifyTVar' (abort queue) $ Just . maybe n (max n)
                  readTVar (envRunning env)
              post $ Incoming req hit
          loop

  post x = atomically $ writeTChan (envRequests env) x

-- | Serve requests until end of input or a @close@ request.
mainLoop :: AgentEnv -> CommandM ()
mainLoop env = do
  incoming <- liftIO $ atomically $ readTChan (envRequests env)
  case incoming of
    EndOfInput -> return ()
    Malformed err -> do
      liftIO $ do
        writeIORef (envCurrentId env) Null
        emit env $ errorLine Null $ agentError ParseError err
      mainLoop env
    Incoming req hit -> do
      liftIO $ writeIORef (envCurrentId env) (reqId req)
      done <- serve env hit req
      unless done $ mainLoop env

-- | Handle one request; return 'True' if the session should end.
serve :: AgentEnv -> Bool -> AgentRequest -> CommandM Bool
serve env hit req = do
  reply <- withDisplayOptions req $ runExceptT $ dispatch env hit req
  liftIO $ emit env $ either (errorLine (reqId req)) (resultLine (reqId req)) reply
  return $ reqMethod req == "close"

-- | Run a request with the printing options it asked for.
--
--   Only the two display flags are put back afterwards, and they are
--   put back into whatever options the request left behind: a load
--   installs the options of the file it checked, and those have to
--   survive.
withDisplayOptions :: AgentRequest -> CommandM a -> CommandM a
withDisplayOptions req m
  | isNothing (reqShowImplicit req), isNothing (reqShowIrrelevant req) = m
  | otherwise = do
      before <- useTC stPragmaOptions
      setTCLens stPragmaOptions $ apply before
      a <- m
      after <- useTC stPragmaOptions
      setTCLens stPragmaOptions $ restore before after
      return a
  where
  apply = flag lensOptShowImplicit (reqShowImplicit req)
        . flag lensOptShowIrrelevant (reqShowIrrelevant req)
  flag l = maybe id (set (l . lensKeepDefault))
  restore before after =
      set lensOptShowImplicit   (before ^. lensOptShowImplicit)
    $ set lensOptShowIrrelevant (before ^. lensOptShowIrrelevant) after

------------------------------------------------------------------------
-- * The agent monad

-- | 'CommandM' with structured failure.
type AgentM = ExceptT AgentError CommandM

-- | Run a 'TCM' query -- a rendering, a lookup -- for its result only:
--   state changes are discarded and errors are reported as internal
--   errors, since a query has no business failing.
readTCM :: TCM a -> AgentM a
readTCM m = do
  r <- lift $ lift $ localTCState $
         (Right <$> m) `catchError` \ e -> Left <$> prettyError e
  case r of
    Right a  -> return a
    Left doc -> throwError $ agentError InternalError $ render doc

-- | Run an @IO@ action, reporting a failure as a structured error
--   rather than letting it tear the frontend down.
liftIOSafe :: String -> IO a -> AgentM a
liftIOSafe what m = liftIO (try m) >>= \case
  Right a -> return a
  Left e  -> throwError $ agentError AgentIOError $
    what ++ " failed: " ++ show (e :: IOException)

-- | Decode the @params@ object of a request.
withParams :: AgentRequest -> (Object -> Parser a) -> AgentM a
withParams req p = case reqParams req of
  Object o -> case parseEither p o of
    Right a  -> return a
    Left err -> throwError $ agentError InvalidParams err
  Null     -> case parseEither p mempty of
    Right a  -> return a
    Left err -> throwError $ agentError InvalidParams err
  _        -> throwError $ agentError InvalidParams "params must be an object"

-- | The @normalise@ parameter, with a per-method default.
normParam :: Rewrite -> Object -> Parser Rewrite
normParam def o = maybe (pure def) parseRewrite =<< o .:? "normalise"

-- | How many hits an answer carries when the request does not say.
defaultNameLimit :: Int
defaultNameLimit = 50

-- | The parameters of a @findNames@ request.  The query itself, and the
--   matching it specifies, live in "Agda.Interaction.NameIndex", since
--   the index file is read by clients that never see this protocol.
nameQueryParams :: Object -> Parser NameQuery
nameQueryParams o = NameQuery
  <$> o .:  "pattern"
  <*> (nonEmpty . T.strip <$> o .:? "module" .!= "")
  <*> o .:? "glob"  .!= False
  <*> (T.words <$> o .:? "type" .!= "")
  <*> o .:? "kinds" .!= []
  <*> o .:? "limit" .!= defaultNameLimit
  where
  nonEmpty t = if T.null t then Nothing else Just t

-- | Run the name-index builder in a process of its own, relaying its
--   output as progress events, and return its exit status.
--
--   A separate process because loading a whole library peaks at about a
--   gigabyte and this one has a proof state to keep responsive; this
--   same binary, because the index format is this Agda's.  It gets this
--   process's own command line, less @--interaction-agent@, so that it
--   resolves libraries as the session does: a @--library-file@ or an
--   @--include-path@ given to the session holds for the index as well.
--   The builder's own error output is left to the frontend's stderr,
--   which is where the daemon's log already goes.
--
--   The build counts as a running command: an @abort@ terminates it,
--   as a timeout does.
runBuilder
  :: AgentEnv
  -> AgentRequest
  -> FilePath        -- ^ The project root, which the builder runs in.
  -> Bool            -- ^ Rebuild from scratch rather than refresh?
  -> AgentM Int
runBuilder env req root full = do
  self  <- liftIOSafe "locating the Agda binary" getExecutablePath
  args  <- filter (/= "--interaction-agent") <$>
             liftIOSafe "reading the command line" getArgs
  i     <- liftIO $ readIORef (envCurrentId env)
  queue <- lift $ gets commandQueue
  n     <- liftIO $ startRunning env
  end   <- liftIOSafe "running the index builder" $
             build self args i n (abort queue)
               `finally` atomically (writeTVar (envRunning env) False)
  case end of
    BuilderExited code -> return code
    BuilderTimedOut    -> throwError $ agentError TimeoutError
      "the index builder did not finish within the request's timeout"
    BuilderAborted     -> throwError $ agentError AbortedError $
      abortMessage AbortedError
  where
  -- 'withCreateProcess' rather than 'createProcess': it closes the
  -- pipe and reaps the child on the way out, including the ways out
  -- that are not the ordinary one -- a timeout, or a write to stdout
  -- that fails while progress is being relayed.
  build self args i n aborts =
    withCreateProcess
      (proc self $
         args ++ [ "--build-name-index=" ++ (if full then "full" else "refresh") ])
      { std_in  = NoStream
      , std_out = CreatePipe
      , std_err = Inherit
      , cwd     = Just root
      }
      \ _ mout _ ph -> do
        aborted <- newIORef False
        -- An abort is read by 'readRequests' and announced through the
        -- command queue, which nothing here otherwise looks at.
        watcher <- forkIO $ do
          atomically $ readTVar aborts >>= \case
            Just a | n <= a -> return ()
            _               -> retry
          writeIORef aborted True
          terminateProcess ph
        let relay h = hIsEOF h >>= \case
              True  -> return ()
              False -> do
                line <- hGetLine h
                emit env $ eventLine i "progress" [ "message" .= line ]
                relay h
            run = do
              mapM_ (\ h -> hSetEncoding h utf8 >> relay h) mout
              exitStatus <$> waitForProcess ph
        result <- (case reqTimeoutMs req of
                     Nothing -> Just <$> run
                     Just ms -> timeout (1000 * ms) run)
                  `finally` killThread watcher
        when (isNothing result) $ do
          terminateProcess ph
          void $ waitForProcess ph
        wasAborted <- readIORef aborted
        return $ if wasAborted then BuilderAborted
                 else maybe BuilderTimedOut BuilderExited result

  exitStatus = \case
    ExitSuccess   -> 0
    ExitFailure c -> c

-- | How a run of the index builder ended.
data BuilderEnd
  = BuilderExited Int  -- ^ With this exit status.
  | BuilderTimedOut
  | BuilderAborted

-- | Hand out the number of the next command, and mark it as running.
startRunning :: AgentEnv -> IO Integer
startRunning env = atomically $ do
  modifyTVar' (envCounter env) succ
  writeTVar (envRunning env) True
  readTVar (envCounter env)

------------------------------------------------------------------------
-- * Running Agda commands

-- | The outcome of handing one 'Interaction' to the type checker.
data Outcome = Outcome
  { outResponses :: [Response]
  , outAborted   :: Maybe AgentErrorCode
      -- ^ 'TimeoutError' or 'AbortedError' if the command did not finish.
  , outTimings   :: Timings
      -- ^ The CPU time the command billed to each account.
  , outChecking  :: Maybe Bench.Account
      -- ^ After a timeout: the account that was being billed when it
      --   fired, which names the definition being checked.
  }

-- | CPU time per account, as "Agda.Benchmarking" bills it.
--
--   The agent frontend always bills definitions (see
--   'Agda.TypeChecking.Monad.Benchmark.updateBenchmarkingStatus'), so
--   these are there without any profiling option; @--profile=modules@
--   or @--profile=internal@ bill to their own accounts instead.
type Timings = Trie.Trie Bench.Phase CPUTime

-- | The timings so far, with the account currently being billed
--   charged up to now, and that account.
--
--   This has to be taken while the command is still running: an abort
--   unwinds every 'Benchmark.billTo', so afterwards nothing is being
--   billed and the definition the command was stuck in is forgotten.
timingsSnapshot :: IO (Timings, Maybe Bench.Account)
timingsSnapshot = do
  b   <- Benchmark.getBenchmark
  now <- getCPUTime
  return $ case Benchmark.currentAccount b of
    Strict.Just (acc, start) ->
      (Trie.insertWith (+) acc (now - start) (Benchmark.timings b), Just acc)
    Strict.Nothing -> (Benchmark.timings b, Nothing)

-- | Run one interaction through the ordinary 'runInteraction' path,
--   with the request's timeout armed.
runCommand :: AgentEnv -> AgentRequest -> FilePath -> Interaction -> AgentM Outcome
runCommand env req file cmd = do
  queue <- lift $ gets commandQueue
  n <- liftIO $ startRunning env
  liftIO $ do
    writeIORef (envResponses env) []
    -- Each command's timings are its own.
    Benchmark.reset
    atomically $ writeTChan (commands queue) (n, Command $ const iotcm)
  before <- lift $ gets theInteractionPoints
  timedOut <- liftIO $ newIORef Nothing
  timer <- liftIO $ forM (reqTimeoutMs req) $ \ ms -> forkIO $ do
    threadDelay $ 1000 * ms
    writeIORef timedOut . Just =<< timingsSnapshot
    atomically $ modifyTVar' (abort queue) $ Just . maybe n (max n)
  -- Billing the command to the root account, as the batch frontend
  -- does, puts the time spent outside any definition under
  -- "Miscellaneous" rather than nowhere.
  result <- lift $ Benchmark.billTo [] $ maybeAbort runInteraction
  liftIO $ do
    atomically $ writeTVar (envRunning env) False
    mapM_ killThread timer
  -- A command that consumed or created goals has moved the session
  -- away from the file on disk; see 'sesDirty'.
  after <- lift $ gets theInteractionPoints
  when (before /= after) $ liftIO $
    modifyIORef' (envSession env) $ \ s -> s{ sesDirty = True }
  responses <- liftIO $ reverse <$> readIORef (envResponses env)
  timings   <- liftIO $ Benchmark.timings <$> Benchmark.getBenchmark
  case result of
    Command (Just ()) -> return $ Outcome responses Nothing timings Nothing
    Command Nothing   -> liftIO (readIORef timedOut) <&> \case
      Just (snapshot, here) -> Outcome responses (Just TimeoutError) snapshot here
      Nothing               -> Outcome responses (Just AbortedError) timings Nothing
    Done              -> throwError $ agentError InternalError
      "the type checker was asked to exit"
    Error err         -> throwError $ agentError InternalError err
  where
  -- The interaction is built here rather than parsed, so all ranges in
  -- it are empty and the top-level module name is irrelevant.
  iotcm = IOTCM file None Direct cmd

-- | Run an interaction and insist that it ran to completion without an
--   Agda error.
runCommandOK :: AgentEnv -> AgentRequest -> FilePath -> Interaction -> AgentM [Response]
runCommandOK env req file cmd = do
  out <- runCommand env req file cmd
  case outAborted out of
    Just code -> throwError $ abortError code out
    Nothing -> case responseError (outResponses out) of
      Just err -> throwAgdaError err
      Nothing  -> return $ outResponses out

-- | What to say about a command that did not run to completion.
abortMessage :: AgentErrorCode -> String
abortMessage = \case
  TimeoutError -> "timed out; the command was aborted and the session is usable"
  _            -> "aborted; the session is usable"

-- | The error for a command that did not run to completion.
--
--   A timeout says what it was checking when it fired, and carries the
--   timings up to then: a load that runs out of time is a load whose
--   slow definitions one wants to know, and without this the only way
--   to find them is to bisect the file by hand.
abortError :: AgentErrorCode -> Outcome -> AgentError
abortError code out = case code of
  TimeoutError -> (agentError code message)
    { aeData = [ "checking" .= name | Just name <- [here] ] ++
               -- Time outside every definition, which is all a
               -- @normalise@ has, says nothing.
               [ "profile" .= profileValue (outTimings out)
               | any (isJust . accountSubject . fst) $ profileRows (outTimings out) ]
    }
  _ -> agentError code $ abortMessage code
  where
  here    = accountSubject =<< outChecking out
  spent   = maybe 0 (\ a -> fromMaybe 0 $ Trie.lookup a (outTimings out)) $
              outChecking out
  message = case here of
    Nothing   -> abortMessage code
    Just name -> concat
      [ "timed out while checking ", name, ", after ", showMs spent
      , " of CPU time on it; the command was aborted and the session is usable"
      ]

------------------------------------------------------------------------
-- * Profiles

-- | What an account bills: a definition, or a module under
--   @--profile=modules@.  The accounts of @--profile=internal@ are
--   phases of the type checker, which do not say where it was.
accountSubject :: Bench.Account -> Maybe String
accountSubject = \case
  Bench.Definition q : _ -> Just $ prettyShow q
  Bench.TopModule m  : _ -> Just $ prettyShow m
  _                      -> Nothing

-- | An account as the batch frontend's table names it.
accountName :: Bench.Account -> String
accountName [] = "Miscellaneous"
accountName ps = List.intercalate "." $ map prettyShow ps

-- | The accounts worth listing, most expensive first: those billed at
--   least 10 ms, the threshold of the batch frontend's table.
profileRows :: Timings -> [(Bench.Account, CPUTime)]
profileRows t = List.sortOn (Down . snd)
  [ r | r@(_, ms) <- Trie.toList t, ms >= CPUTime (10 * 1000000000) ]

profileTotal :: Timings -> CPUTime
profileTotal = sum

milliseconds :: CPUTime -> Integer
milliseconds (CPUTime ps) = ps `div` 1000000000

showMs :: CPUTime -> String
showMs t = show (milliseconds t) ++ " ms"

profileValue :: Timings -> Value
profileValue t = object
  [ "totalMs"  .= milliseconds (profileTotal t)
  , "accounts" .=
      [ object $ [ "account" .= accountName a, "ms" .= milliseconds ms ] ++
                 [ "definition" .= s | Just s <- [accountSubject a] ]
      | (a, ms) <- profileRows t ]
  ]

-- | The profile as a table, at most 'profileTextRows' rows of it.
profileText :: Timings -> String
profileText t = List.intercalate "\n" $
  [ "Profile (CPU time; accounts of 10 ms or more):"
  , row "Total" (profileTotal t)
  ] ++ map (uncurry (row . accountName)) shown ++
  [ "  … and " ++ show (length rest) ++ " more" | not (List.null rest) ]
  where
  (shown, rest) = splitAt profileTextRows $ profileRows t
  width = maximum $ 5 : map (length . accountName . fst) shown
  row name ms = concat
    [ "  ", name, replicate (width - length name) ' '
    , "  ", pad 10 (showMs ms) ]
  pad n s = replicate (n - length s) ' ' ++ s

profileTextRows :: Int
profileTextRows = 30

-- | The first error reported by a command, if any.
responseError :: [Response] -> Maybe Info_Error
responseError rs = listToMaybe [ e | Resp_DisplayInfo (Info_Error e) <- rs ]

-- | Turn an Agda error into a failed request.
throwAgdaError :: Info_Error -> AgentM a
throwAgdaError err = throwError . diagnosticError =<< readTCM (infoErrorDiagnostic err)

-- | An Agda error as the error of a request.
diagnosticError :: Diagnostic -> AgentError
diagnosticError d = (agentError AgdaError (diagMessage d))
  { aeRange    = diagRange d
  , aeAgdaCode = diagCode d
  , aeRelated  = map toJSON (diagRelated d)
  }

-- | The first 'DisplayInfo' matching the given pattern.
findInfo :: (DisplayInfo -> Maybe a) -> [Response] -> Maybe a
findInfo p rs = listToMaybe [ a | Resp_DisplayInfo i <- rs, Just a <- [p i] ]

-- | The first 'DisplayInfo' matching the pattern; a command that does
--   not produce one has not done what this frontend thinks it does.
expectInfo :: (DisplayInfo -> Maybe a) -> String -> [Response] -> AgentM a
expectInfo p what = maybe (missingInfo what) return . findInfo p

-- | Complain about a command that did not produce the expected response.
missingInfo :: String -> AgentM a
missingInfo what = throwError $ agentError InternalError $
  "the type checker did not report " ++ what

-- | The goals and warnings that a load, and every mutation, reports.
allGoalsInfo :: DisplayInfo -> Maybe (Goals, WarningsAndNonFatalErrors)
allGoalsInfo = \case
  Info_AllGoalsWarnings ms wes -> Just (ms, wes)
  _ -> Nothing

------------------------------------------------------------------------
-- * Sessions, revisions and resynchronisation

session :: AgentEnv -> AgentM AgentSession
session env = liftIO $ readIORef (envSession env)

-- | The file the session is about.
currentFile :: AgentEnv -> AgentM AbsolutePath
currentFile env = do
  s <- session env
  case sesFile s of
    Just f  -> return f
    Nothing -> throwError $ agentError NoFileLoaded
      "no file is loaded; call the load method first"

-- | The directory the name index is looked up from: that of the
--   session's file, or, before anything is loaded, the one the frontend
--   runs in -- @agda-client@ starts it in the project root, so a lemma
--   can be looked for before the file that would use it exists.
projectDir :: AgentEnv -> AgentM FilePath
projectDir env = session env >>= \ s -> case sesFile s of
  Just f  -> return $ takeDirectory $ filePath f
  Nothing -> liftIOSafe "finding the working directory" getCurrentDirectory

-- | Type-check a file and make it the session's file.
--
--   The file becomes the session's file even if the check fails, so
--   that the client can query and reload it afterwards.
loadFile :: AgentEnv -> AgentRequest -> AbsolutePath -> [String] -> AgentM [Response]
loadFile env req file args = fst <$> loadFileWith env req file args []

-- | 'loadFile' with extra Agda arguments for this load only: they are
--   not remembered for the reloads that follow.  Also returns the
--   load's timings.
loadFileWith
  :: AgentEnv -> AgentRequest -> AbsolutePath -> [String] -> [String]
  -> AgentM ([Response], Timings)
loadFileWith env req file args extra =
  loadFile'' env req file args extra >>= \case
    (Right rs, t) -> return (rs, t)
    (Left d, _)   -> throwError $ diagnosticError d

-- | 'loadFile' without the last step: an Agda error is returned rather
--   than thrown.  A reload that follows a mutation needs this, because
--   the source has already been rewritten and the client has to hear
--   about that even when the result does not type-check.
loadFile'
  :: AgentEnv -> AgentRequest -> AbsolutePath -> [String]
  -> AgentM (Either Diagnostic [Response])
loadFile' env req file args = fst <$> loadFile'' env req file args []

-- | 'loadFile'' with extra Agda arguments for this load only, and the
--   load's timings.
loadFile''
  :: AgentEnv -> AgentRequest -> AbsolutePath -> [String] -> [String]
  -> AgentM (Either Diagnostic [Response], Timings)
loadFile'' env req file args extra = do
  -- Hash the source we are about to check, so that a query issued
  -- after an edit that raced with the load still resyncs.
  h   <- liftIOSafe "reading the source" $ hashTextFile file
  out <- runCommand env req (filePath file) $
           Cmd_load (filePath file) (args ++ extra)
  failure <- traverse (readTCM . infoErrorDiagnostic) $
               responseError $ outResponses out
  liftIO $ modifyIORef' (envSession env) $ \ s -> s
    { sesFile     = Just file
    , sesArgs     = args
    , sesHash     = Just h
    , sesRevision = sesRevision s + 1
    , sesChecked  = isNothing failure && isNothing (outAborted out)
    , sesFailure  = failure
    , sesDirty    = False
    }
  case outAborted out of
    Just code -> throwError $ abortError code out
    Nothing   -> return
      (maybe (Right (outResponses out)) Left failure, outTimings out)

-- | A load that profiles: the file is checked with these profiling
--   options, from source even if its interface is current, and the
--   result carries the timings.
--
--   Agda reuses a current interface for the main module too, and a
--   reused interface checks nothing, so its profile would be empty;
--   removing the interface file is what touching the source would
--   achieve, without touching it.  Imported modules keep their
--   interfaces: a profile of the whole project is a matter of
--   @--ignore-interfaces@, which the caller can pass in @args@.
profiledLoad
  :: AgentEnv -> AgentRequest -> AbsolutePath -> [String] -> [String]
  -> AgentM Value
profiledLoad env req file args profile = do
  unless (List.null profile) $ do
    ifile <- readTCM $ fmap filePath . toIFile =<< srcFromPath file
    liftIOSafe "removing the interface file" $
      removeFile ifile `catch` \ e ->
        unless (isDoesNotExistError e) $ throwIO e
  (rs, timings) <- loadFileWith env req file args
                     [ "--profile=" ++ p | p <- profile ]
  result <- loadResult env rs noResync
  -- A profiling option the file or the session's arguments set gets
  -- its table too; the definitions the frontend bills of its own
  -- accord do not.
  profiling <- readTCM $ (\ o -> any (`containsProfileOption` o)
                 [Internal, Modules, Definitions]) . optProfiling <$> pragmaOptions
  return $ if List.null profile && not profiling then result else
    mergeObject result
      [ "profile" .= profileValue timings
      , "text"    .= (fromMaybe "" (resultText result) ++ "\n\n" ++ profileText timings)
      ]

-- | The @profile@ parameter of @load@ and @reload@: one profiling
--   option, a list of them, or @true@ for @definitions@.
profileParam :: Object -> Parser [String]
profileParam o = o .:? "profile" >>= \case
  Nothing         -> pure []
  Just (Bool b)   -> pure [ "definitions" | b ]
  Just (String t) -> pure [ T.unpack t ]
  Just v          -> parseJSON v

-- | Why a query reloaded the file before answering.
data Resync = Resync
  { resChanged  :: Bool  -- ^ The file on disk differs from what was loaded.
  , resReloaded :: Bool  -- ^ A load was run before answering.
  }

noResync :: Resync
noResync = Resync False False

-- | Bring the session in sync with the file on disk before answering a
--   query.
--
--   Reloading is also needed when the session state no longer matches
--   the source: the 'CommandState' lost the current file, which happens
--   after an abort, or a mutation gave up after changing the proof
--   state but before writing ('sesDirty').
--
--   A load that failed also leaves no current file, but reloading an
--   unchanged file would only fail the same way at the same cost, so
--   the query is answered with the error the load stopped at
--   ('sesFailure') until the file changes.
resync :: AgentEnv -> AgentRequest -> AgentM Resync
resync env req = do
  file <- currentFile env
  s    <- session env
  h    <- liftIOSafe "reading the source" $ hashTextFile file
  cf   <- lift $ gets theCurrentFile
  let changed    = Just h /= sesHash s
      stateLost  = isNothing cf || sesDirty s
  case reqExpectRevision req of
    Just n | changed || n /= sesRevision s ->
      throwError $ agentErrorData StaleDocument
        (concat $
          [ "the document is at revision " ++ show (sesRevision s) ] ++
          [ ", not " ++ show n | n /= sesRevision s ] ++
          [ " and has changed on disk since it was loaded" | changed ])
        [ "revision" .= sesRevision s, "changedOnDisk" .= changed ]
    _ -> return ()
  case sesFailure s of
    Just d | not changed -> throwError (diagnosticError d)
      { aeData = [ "revision" .= sesRevision s, "unchangedSinceLoad" .= True ] }
    _ | changed || stateLost ->
          Resync changed True <$ loadFile env req file (sesArgs s)
      | otherwise -> return noResync

-- | Fields every result carries.
sessionFields :: AgentEnv -> Resync -> AgentM [Pair]
sessionFields env r = do
  s <- session env
  return
    [ "file"     .= (filePath <$> sesFile s)
    , "revision" .= sesRevision s
    , "checked"  .= sesChecked s
      -- The file had changed on disk and was reloaded.
    , "resynced" .= resChanged r
      -- A reload happened, for whichever reason.
    , "reloaded" .= resReloaded r
    ]

------------------------------------------------------------------------
-- * Goals

-- | The goals of the session, in file order.
goalList :: AgentM [(InteractionId, Range)]
goalList = do
  iis <- lift $ gets theInteractionPoints
  forM iis $ \ ii -> (ii,) <$> readTCM (getInteractionRange ii)

-- | Resolve a goal address against the current goals.
--
--   Only a position needs the goals' ranges; an identifier is looked up
--   in the interaction points themselves.
resolveGoal :: GoalRef -> AgentM InteractionId
resolveGoal = \case
  GoalById n -> do
    iis <- lift $ gets theInteractionPoints
    let ii = InteractionId n
    if ii `elem` iis then return ii
    else unknown $ "there is no goal ?" ++ show n
  GoalAtPos line col -> do
    goals <- goalList
    case [ ii | (ii, r) <- goals, covers line col r ] of
      ii : _ -> return ii
      []     -> unknown $ concat
        [ "there is no goal at line ", show line, ", column ", show col ]
  where
  -- The goals that do exist are what the client needs to recover.
  unknown msg = do
    goals <- goalList
    throwError $ agentErrorData UnknownGoal msg
      [ "goals" .=
          [ object [ "id" .= interactionId ii, "range" .= agentRange r ]
          | (ii, r) <- goals ] ]

-- | Is the given 1-based line and column inside the range?
covers :: Word32 -> Word32 -> Range -> Bool
covers line col r = any inside $ rangeIntervals r
  where
  inside i = at (iStart i) <= (line, col) && (line, col) <= at (iEnd i)
  at p = (posLine p, posCol p)

-- | Identify and locate a goal.
goalFields :: InteractionId -> AgentM [Pair]
goalFields ii = do
  r <- readTCM $ getInteractionRange ii
  return [ "goal" .= object [ "id" .= interactionId ii, "range" .= agentRange r ] ]

------------------------------------------------------------------------
-- * Encoding pieces of Agda's answers

-- | Goals and warnings, structured and rendered.
encodeGoals :: Goals -> WarningsAndNonFatalErrors -> AgentM [Pair]
encodeGoals ms@(visible, hidden) wes = do
  gs <- forM visible $ \ oc -> do
    let ii = B.outputFormId $ OutputForm noRange [] alwaysUnblock oc
    ty <- readTCM $ withInteractionId ii $ render <$> goalTypeOf oc
    r  <- readTCM $ getInteractionRange ii
    return $ object
      [ "id" .= interactionId ii, "type" .= ty, "range" .= agentRange r ]
  hs <- forM hidden $ \ oc -> do
    let m = nmid $ namedMetaOf oc
    d <- readTCM $ withMetaId m $ render <$> prettyATop oc
    r <- readTCM $ getMetaRange m
    return $ object [ "text" .= d, "range" .= agentRange r ]
  ws <- readTCM $
          mapM warningDiagnostic =<< filterTCWarnings (tcWarnings wes)
  es <- readTCM $
          mapM nonFatalErrorDiagnostic =<< filterTCWarnings (nonFatalErrors wes)
  doc <- readTCM $ prettyGoals ms
  return
    [ "goals"       .= gs
    , "hiddenGoals" .= hs
    , "warnings"    .= map diagnosticValue ws
    , "errors"      .= map diagnosticValue es
    , "text"        .= render
        (goalsDoc (length visible) doc (map diagMessage ws) (map diagMessage es))
    ]
  where
  goalTypeOf :: OutputConstraint A.Expr InteractionId -> TCM Doc
  goalTypeOf = \case
    OfType _ e -> prettyATop e
    oc         -> prettyATop oc

-- | The rendering of a goal list: a one-line summary, then the goals,
--   then errors and warnings.
goalsDoc :: Int -> Doc -> [String] -> [String] -> Doc
goalsDoc n goals ws es = vcat $ concat
  [ [ text $ concat
        [ show n, " goal", if n == 1 then "" else "s"
        , if null ws then "" else
            ", " ++ show (length ws) ++ " warning" ++ (if length ws == 1 then "" else "s")
        , if null es then "" else
            ", " ++ show (length es) ++ " error" ++ (if length es == 1 then "" else "s")
        ] ]
  , [ goals | not $ null goals ]
  , map text es
  , map text ws
  ]

-- | The goal type display, structured and rendered.
encodeGoalType
  :: InteractionId -> Rewrite -> GoalTypeAux -> [ResponseContextEntry]
  -> [IPFace' C.Expr] -> [OutputForm C.Expr C.Expr] -> AgentM [Pair]
encodeGoalType ii norm aux ctx bndry constraints = do
  ty   <- readTCM $ withInteractionId ii $ render <$> prettyTypeOfMeta norm ii
  cs   <- readTCM $ withInteractionId ii $ mapM contextEntryValue ctx
  doc  <- readTCM $ withInteractionId ii $
            prettyGoalType ii norm aux ctx bndry constraints
  extra <- case aux of
    GoalOnly              -> return []
    GoalAndHave e _       ->
      (\ d -> [ "have" .= d ]) <$> readTCM (withInteractionId ii $ render <$> prettyATop e)
    GoalAndElaboration e  ->
      (\ d -> [ "elaborated" .= d ]) <$> readTCM (withInteractionId ii $ render <$> prettyATop e)
  return $
    [ "type"        .= ty
    , "context"     .= cs
    , "boundary"    .= map (render . pretty) bndry
    , "constraints" .= map (render . pretty) constraints
    , "text"        .= render doc
    ] ++ extra

------------------------------------------------------------------------
-- * Mutations

-- | The text that replaces a hole, according to a give-like command.
--
--   'Give_Paren' and 'Give_NoParen' mean "keep the expression that was
--   given", parenthesised or not.  For the Emacs frontend that is the
--   text inside the goal; here it is the expression from the request,
--   which is the one Agda checked.
giveActionText :: InteractionId -> String -> [Response] -> AgentM TL.Text
giveActionText ii expr rs =
  case [ g | Resp_GiveAction ii' g <- rs, ii' == ii ] of
    g : _ -> return $ case g of
      Give_String s -> TL.pack s
      Give_Paren    -> "(" <> TL.pack (trim expr) <> ")"
      Give_NoParen  -> TL.pack (trim expr)
    [] -> case findInfo nothingToDo rs of
      Just (msg, extra) -> throwError $ agentErrorData NotApplicable msg extra
      Nothing           -> missingInfo "a give action"
  where
  nothingToDo = \case
    Info_Intro_NotFound ->
      Just ("no introduction form fits this goal", [])
    Info_Intro_ConstructorUnknown ss ->
      Just ( "several constructors fit this goal; give one of them explicitly"
           , [ "candidates" .= ss ] )
    _ -> Nothing

-- | Plan the replacement of a hole, or say why it cannot be done.
planEdit :: Snapshot -> Range -> TL.Text -> AgentM TextEdit
planEdit snap hole new = case planHoleEdit snap hole new of
  Right ed -> return ed
  Left err -> throwError $ agentErrorAt InternalError
    ("the result cannot be applied to the source: " ++ err) (agentRange hole)

-- | Plan the replacement of a clause by the result of a case split.
planClause :: Snapshot -> Range -> Range -> Bool -> [String] -> AgentM TextEdit
planClause snap clause hole extLam cs =
  case planClauseEdit snap clause hole extLam (map TL.pack cs) of
    Right ed -> return ed
    Left err -> throwError $ agentErrorAt InternalError
      ("the split cannot be applied to the source: " ++ err) (agentRange clause)

-- | Rewrite the source, reload, and report the resulting proof state.
--
--   The snapshot must have been taken before the command that produced
--   the edits ran, and the list of edits must not be empty.
mutationResult
  :: AgentEnv -> AgentRequest -> Resync -> Snapshot
  -> [String]     -- ^ What was applied, one line each.
  -> [Pair]       -- ^ Extra result fields.
  -> [TextEdit]
  -> AgentM Value
mutationResult env req resynced snap summary extra edits = do
  case checkOverlaps edits of
    Left err -> throwError $ agentError InternalError err
    Right () -> return ()
  -- The command may have taken a while; do not write over an edit the
  -- client made in the meantime.
  untouched <- liftIOSafe "reading the source" $ snapshotUnchanged snap
  unless untouched $ throwError $ agentErrorData StaleDocument
    "the file changed on disk while the command was running; nothing was written"
    [ "edits" .= map editValue edits ]
  before <- sesRevision <$> session env
  liftIOSafe "writing the source" $ writeSource snap $ applyEdits snap edits
  file <- currentFile env
  s    <- session env
  rs   <- loadFile' env req file (sesArgs s)
  fields <- sessionFields env resynced{ resReloaded = True }
  let common = fields ++ extra ++
        [ "applied"          .= True
        , "edits"            .= map editValue edits
        , "previousRevision" .= before
        ]
  case rs of
    Right responses -> do
      (ms, wes) <- expectInfo allGoalsInfo "the goal list" responses
      gs <- encodeGoals ms wes
      return $ object $ common ++ [ p | p@(k, _) <- gs, k /= "text" ] ++
        [ "text" .= List.intercalate "\n" (summary ++ [ renderedText gs ]) ]
    Left d ->
      return $ object $ common ++
        [ "goals"       .= ([] :: [Value])
        , "hiddenGoals" .= ([] :: [Value])
        , "warnings"    .= ([] :: [Value])
        , "errors"      .= [ diagnosticValue d ]
        , "text"        .= List.intercalate "\n"
            (summary ++ [ "the file no longer type-checks:", diagMessage d ])
        ]
  where
  renderedText ps = case lookup "text" ps of
    Just (String t) -> T.unpack t
    _               -> ""

-- | Run a command that fills a single goal, and apply its result.
giveLike
  :: AgentEnv -> AgentRequest
  -> String                          -- ^ The verb, for the rendering.
  -> GoalRef
  -> (InteractionId -> Interaction)
  -> String                          -- ^ The expression the client gave.
  -> AgentM Value
giveLike env req verb ref cmd expr = do
  resynced <- resync env req
  file <- currentFile env
  ii   <- resolveGoal ref
  -- Both of these have to be taken before the command runs: giving
  -- removes the interaction point, and the snapshot is the source the
  -- command's ranges refer to.
  hole <- readTCM $ getInteractionRange ii
  snap <- liftIOSafe "reading the source" $ takeSnapshot file
  rs   <- runCommandOK env req (filePath file) $ cmd ii
  new  <- giveActionText ii expr rs
  ed   <- planEdit snap hole new
  mutationResult env req resynced snap
    [ verb ++ " ?" ++ show (interactionId ii) ++ " = " ++ TL.unpack (edNew ed) ]
    [ "goal" .= object [ "id" .= interactionId ii, "range" .= agentRange hole ] ]
    [ ed ]

-- | The range of a goal that was in the goal list when the command started.
holeRange :: [(InteractionId, Range)] -> InteractionId -> AgentM Range
holeRange goals ii = case lookup ii goals of
  Just r  -> return r
  Nothing -> throwError $ agentError InternalError $
    "the type checker filled ?" ++ show (interactionId ii) ++ ", which is not a goal"

trim :: String -> String
trim = dropWhile isSpace . List.dropWhileEnd isSpace

------------------------------------------------------------------------
-- * Encoding the phase-4 queries

-- | One checked candidate: the expression, and either the error that
--   rejected it or the term it elaborated to.
type CheckedCandidate = (String, Either Diagnostic String)

candidateValue :: CheckedCandidate -> Value
candidateValue (expr, r) = object $ ("expr" .= expr) : case r of
  Left  d  -> [ "accepted" .= False, "error" .= diagnosticValue d ]
  Right el -> [ "accepted" .= True,  "elaborated" .= el ]

candidatesText :: [CheckedCandidate] -> String
candidatesText cands = List.intercalate "\n" $ summary : map line cands
  where
  accepted = length [ () | (_, Right _) <- cands ]
  summary  = concat
    [ show accepted, " of ", show (length cands), " accepted" ]
  line (expr, Right el)
    | el == trim expr = "accepted: " ++ expr
    | otherwise       = "accepted: " ++ expr ++ "\n  elaborates to " ++ el
  line (expr, Left d) =
    "rejected: " ++ expr ++ "\n" ++ indent (diagMessage d)
  indent = List.intercalate "\n" . map ("  " ++) . lines

-- | The @[{name, type}]@ a client sees.
namedTypesValue :: [(String, Doc)] -> Value
namedTypesValue entries =
  toJSON [ object [ "name" .= n, "type" .= render d ] | (n, d) <- entries ]

-- | The name of the syntactic aspect at a position, for a client that
--   would rather dispatch on a word than on a rendering.
aspectName :: Maybe Asp.Aspect -> Text
aspectName = \case
  Nothing                -> "unknown"
  Just (Asp.Name mk _)   -> maybe "name" (T.pack . nameKindName) mk
  Just Asp.Comment       -> "comment"
  Just Asp.Keyword       -> "keyword"
  Just Asp.String        -> "string"
  Just Asp.Number        -> "number"
  Just Asp.Hole          -> "hole"
  Just Asp.Symbol        -> "symbol"
  Just Asp.PrimitiveType -> "primitiveType"
  Just Asp.Pragma        -> "pragma"
  Just Asp.Background    -> "background"
  Just Asp.Markup        -> "markup"
  Just (Asp.URL _)       -> "url"
  where
  -- 'NameKind' has no printer of its own; its constructors are the
  -- names, with the constructor's induction spelled out.
  nameKindName :: Asp.NameKind -> String
  nameKindName k = case words (show k) of
    ["Constructor", ind] -> lower ind ++ "Constructor"
    w : _                -> lower w
    []                   -> "name"
  lower (c : cs) = toLower c : cs
  lower []       = []

-- | The highlighting entry covering a position, if there is one.
--
--   Positions here are Agda's: code points counted from 1.
aspectsAt :: HighlightingInfo -> Int -> Maybe (HRange.Range, Aspects)
aspectsAt hi p = List.find covered $ toList hi
  where
  covered (r, _) = HRange.from r <= p && p < HRange.to r

-- | The source text of a range given by Agda positions.
sourceText :: Snapshot -> Int -> Int -> String
sourceText snap from to =
  TL.unpack $ TL.take (fromIntegral (to - from)) $
    TL.drop (fromIntegral (from - 1)) $ snapText snap

-- | A range given by Agda positions, as the protocol reports ranges.
rangeInFile :: AbsolutePath -> Snapshot -> Int -> Int -> Value
rangeInFile file snap from to = object
  [ "file"  .= filePath file
  , "start" .= at from
  , "end"   .= at to
  ]
  where
  at p = let (l, c) = lineColOfOffset (snapText snap) (fromIntegral p - 1)
         in object [ "line" .= l, "col" .= c, "offset" .= (p - 1) ]

-- | Where a definition site is, structured and rendered.
--
--   The site is a position in the defining module, which may be a
--   different file — a library one, say.  That file is read to turn the
--   position into a line and a column; if it cannot be read, the
--   position is still reported.
definitionSiteValue
  :: AbsolutePath -> Snapshot -> ModuleToSource -> DefinitionSite
  -> AgentM (Value, String)
definitionSiteValue file snap m2s (DefinitionSite m p _ anchor) = do
  msnap <- case moduleFile m2s m of
    Nothing                -> return Nothing
    Just path | path == file -> return $ Just (path, Just snap)
              | otherwise  -> do
                  r <- liftIO $ try $ takeSnapshot path
                  return $ Just (path, either (\ (_ :: IOException) -> Nothing) Just r)
  let place = case msnap of
        Just (path, Just s) ->
          let (l, c) = lineColOfOffset (snapText s) (fromIntegral p - 1)
          in ( [ "file" .= filePath path, "line" .= l, "col" .= c ]
             , filePath path ++ ":" ++ show l ++ "," ++ show c )
        Just (path, Nothing) ->
          ( [ "file" .= filePath path ], filePath path )
        Nothing -> ( [], prettyShow m )
  return
    ( object $ fst place ++
        [ "module" .= prettyShow m
        , "offset" .= (p - 1)
        , "anchor" .= anchor
        ]
    , "defined at " ++ snd place
    )

-- | The file a module was read from, if the session knows it.
moduleFile :: ModuleToSource -> TopLevelModuleName -> Maybe AbsolutePath
moduleFile m2s m
  | Map.member m (moduleToSourceId m2s) = Just $ topLevelModuleFilePath m2s m
  | otherwise                           = Nothing

------------------------------------------------------------------------
-- * Methods

-- | Answer one request.  The 'Bool' is only consulted by @abort@: did
--   it interrupt a running command?
dispatch :: AgentEnv -> Bool -> AgentRequest -> AgentM Value
dispatch env hit req = case reqMethod req of

  "handshake" -> do
    fields <- sessionFields env noResync
    return $ object $ ("handshake" .= handshakeValue) : fields

  "abort" -> do
    fields <- sessionFields env noResync
    return $ object $ fields ++
      [ "aborted" .= hit
      , "text"    .= (if hit then "aborted the running command"
                      else "nothing was running" :: Text)
      ]

  "close" -> do
    fields <- sessionFields env noResync
    return $ object $ fields ++
      [ "closed" .= True, "text" .= ("session closed" :: Text) ]

  "status" -> do
    goals  <- goalList
    fields <- sessionFields env noResync
    s      <- session env
    let text' = List.intercalate "\n" $
          [ "agda:     " ++ versionWithCommitInfo
          , "file:     " ++ maybe "(none loaded)" filePath (sesFile s)
          , "revision: " ++ show (sesRevision s)
          , "checked:  " ++ show (sesChecked s)
          , "goals:    " ++ List.intercalate " "
              [ '?' : show (interactionId ii) | (ii, _) <- goals ]
          ]
    return $ object $ fields ++
      [ "agdaVersion" .= versionWithCommitInfo
      , "goals" .= [ object [ "id" .= interactionId ii, "range" .= agentRange r ]
                   | (ii, r) <- goals ]
      , "text" .= text'
      ]

  "load" -> do
    (file, args, profile) <- withParams req $ \ o ->
      (,,) <$> o .: "file" <*> o .:? "args" .!= [] <*> profileParam o
    path <- liftIO $ absolute file
    profiledLoad env req path args profile

  "reload" -> do
    profile <- withParams req profileParam
    file    <- currentFile env
    s       <- session env
    profiledLoad env req file (sesArgs s) profile

  "goals" -> do
    norm     <- withParams req $ normParam AsIs
    resynced <- resync env req
    file     <- currentFile env
    rs       <- runCommandOK env req (filePath file) $ Cmd_metas norm
    (ms, wes) <- expectInfo allGoalsInfo "the goal list" rs
    fields <- sessionFields env resynced
    gs     <- encodeGoals ms wes
    return $ object $ fields ++ gs

  "goal" -> do
    (ref, norm) <- withParams req $ \ o ->
      (,) <$> o .: "goal" <*> normParam Simplified o
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    rs   <- runCommandOK env req (filePath file) $
              Cmd_goal_type_context norm ii noRange ""
    goalTypeResult env resynced ii rs

  "context" -> do
    (ref, norm) <- withParams req $ \ o ->
      (,) <$> o .: "goal" <*> normParam Simplified o
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    rs   <- runCommandOK env req (filePath file) $ Cmd_context norm ii noRange ""
    ctx  <- expectInfo (contextOf ii) "the context" rs
    entries <- readTCM $ withInteractionId ii $ mapM contextEntryValue ctx
    doc     <- readTCM $ localTCState $ prettyResponseContext ii False ctx
    fields  <- sessionFields env resynced
    gf      <- goalFields ii
    return $ object $ fields ++ gf ++
      [ "context" .= entries
      , "text"    .= render doc
      ]

  -- Checking a candidate is non-mutating: whether it is accepted or
  -- rejected, the goal is still there afterwards.  A rejected candidate
  -- is a result, not a failed request.
  "check" -> do
    (ref, expr, norm) <- withParams req $ \ o ->
      (,,) <$> o .: "goal" <*> o .: "expr" <*> normParam Simplified o
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    out  <- runCommand env req (filePath file) $
              Cmd_goal_type_context_check norm ii noRange expr
    case outAborted out of
      Just code -> throwError $ agentError code "the check was aborted"
      Nothing -> case responseError (outResponses out) of
        Just err -> do
          d      <- readTCM $ infoErrorDiagnostic err
          fields <- sessionFields env resynced
          gf     <- goalFields ii
          return $ object $ fields ++ gf ++
            [ "accepted" .= False
            , "error"    .= diagnosticValue d
            , "text"     .= ("rejected\n" ++ diagMessage d)
            ]
        Nothing -> do
          result <- goalTypeResult env resynced ii (outResponses out)
          return $ mergeObject result
            [ "accepted" .= True
            , "text"     .= ("accepted\n" ++ maybe "" id (resultText result))
            ]

  "infer" -> do
    (mref, expr, norm) <- withParams req $ \ o ->
      (,,) <$> o .:? "goal" <*> o .: "expr" <*> normParam Simplified o
    resynced <- resync env req
    file     <- currentFile env
    fields   <- sessionFields env resynced
    case mref of
      Just ref -> do
        ii <- resolveGoal ref
        rs <- runCommandOK env req (filePath file) $ Cmd_infer norm ii noRange expr
        e  <- expectInfo (inferredInGoal ii) "an inferred type" rs
        d  <- readTCM $ withInteractionId ii $ render <$> prettyATop e
        gf <- goalFields ii
        return $ object $ fields ++ gf ++
          [ "scope" .= ("goal" :: Text), "type" .= d, "text" .= d ]
      Nothing -> do
        rs <- runCommandOK env req (filePath file) $ Cmd_infer_toplevel norm expr
        (st, e) <- expectInfo inferredAtTop "an inferred type" rs
        d <- readTCM $ render <$> prettyInferredType st e
        return $ object $ fields ++
          [ "scope" .= ("topLevel" :: Text), "type" .= d, "text" .= d ]

  "normalise" -> do
    (mref, expr, mode) <- withParams req $ \ o -> (,,)
      <$> o .:? "goal"
      <*> o .: "expr"
      <*> (maybe (pure DefaultCompute) parseComputeMode =<< o .:? "mode")
    resynced <- resync env req
    file     <- currentFile env
    fields   <- sessionFields env resynced
    let modeField = "mode" .= computeModeName mode
    case mref of
      Just ref -> do
        ii <- resolveGoal ref
        rs <- runCommandOK env req (filePath file) $ Cmd_compute mode ii noRange expr
        e  <- expectInfo (normalFormInGoal ii) "a normal form" rs
        d  <- readTCM $ withInteractionId ii $ render <$> B.showComputed mode e
        gf <- goalFields ii
        return $ object $ fields ++ gf ++
          [ "scope" .= ("goal" :: Text), modeField
          , "result" .= d, "text" .= d ]
      Nothing -> do
        rs <- runCommandOK env req (filePath file) $ Cmd_compute_toplevel mode expr
        (st, e) <- expectInfo normalFormAtTop "a normal form" rs
        d <- readTCM $ render <$> prettyNormalForm st mode e
        return $ object $ fields ++
          [ "scope" .= ("topLevel" :: Text), modeField
          , "result" .= d, "text" .= d ]

  -- Mutating methods.  Each rewrites the file on disk and reloads it,
  -- so the goals it reports are those of the new revision.

  "give" -> do
    (ref, expr, force) <- withParams req $ \ o -> (,,)
      <$> o .: "goal" <*> o .: "expr" <*> o .:? "force" .!= False
    giveLike env req "give" ref
      (\ ii -> Cmd_give (if force then WithForce else WithoutForce) ii noRange expr)
      expr

  "elaborate" -> do
    (ref, expr, norm) <- withParams req $ \ o -> (,,)
      <$> o .: "goal" <*> o .: "expr" <*> normParam Normalised o
    giveLike env req "elaborate" ref
      (\ ii -> Cmd_elaborate_give norm ii noRange expr) expr

  -- Refining with an empty expression is introduction, which is what
  -- the Emacs mode's refine command does as well.
  "refine" -> do
    (ref, expr, pm) <- withParams req $ \ o -> (,,)
      <$> o .: "goal" <*> o .:? "expr" .!= "" <*> o .:? "patternLambda" .!= False
    giveLike env req "refine" ref
      (\ ii -> Cmd_refine_or_intro pm ii noRange expr) expr

  "intro" -> do
    (ref, pm) <- withParams req $ \ o -> (,)
      <$> o .: "goal" <*> o .:? "patternLambda" .!= False
    giveLike env req "intro" ref (\ ii -> Cmd_intro pm ii noRange "") ""

  -- Write out the goals the type checker has already solved by
  -- unification.  Agda prints those solutions without regard for the
  -- precedence at the hole, so each one is handed back to 'Cmd_give',
  -- which parenthesises it as needed; this is what the Emacs mode does.
  "solve" -> do
    (mref, norm) <- withParams req $ \ o -> (,)
      <$> o .:? "goal" <*> normParam Instantiated o
    resynced <- resync env req
    file  <- currentFile env
    mii   <- traverse resolveGoal mref
    goals <- goalList
    snap  <- liftIOSafe "reading the source" $ takeSnapshot file
    rs    <- runCommandOK env req (filePath file) $
               maybe (Cmd_solveAll norm) (\ ii -> Cmd_solveOne norm ii noRange "") mii
    let sols = concat [ ss | Resp_SolveAll ss <- rs ]
    when (List.null sols) $ throwError $ agentError NotApplicable $ case mii of
      Just ii -> "goal ?" ++ show (interactionId ii) ++ " is not solved yet"
      Nothing -> "no goal has been solved by the type checker"
    edits <- forM sols $ \ (ii, e) -> do
      let txt = prettyShow e
      hole <- holeRange goals ii
      gs   <- runCommandOK env req (filePath file) $
                Cmd_give WithoutForce ii noRange txt
      new  <- giveActionText ii txt gs
      (,) ii <$> planEdit snap hole new
    mutationResult env req resynced snap
      [ "solve ?" ++ show (interactionId ii) ++ " = " ++ TL.unpack (edNew ed)
      | (ii, ed) <- edits ]
      [] (map snd edits)

  "auto" -> do
    (mref, args, norm) <- withParams req $ \ o -> (,,)
      <$> o .:? "goal" <*> o .:? "args" .!= "" <*> normParam AsIs o
    resynced <- resync env req
    file  <- currentFile env
    mii   <- traverse resolveGoal mref
    goals <- goalList
    snap  <- liftIOSafe "reading the source" $ takeSnapshot file
    rs    <- runCommandOK env req (filePath file) $
               maybe (Cmd_autoAll norm) (\ ii -> Cmd_autoOne norm ii noRange args) mii
    let given    = [ (ii, s) | Resp_GiveAction ii (Give_String s) <- rs ]
        messages = [ m | Resp_DisplayInfo (Info_Auto m) <- rs ]
    if List.null given then
      -- Proof search that only lists candidates (@args@ asked for it) is
      -- a result; one that found nothing is not.
      case [ m | m <- messages, solutionsHeader `List.isPrefixOf` m ] of
        m : _ -> do
          fields <- sessionFields env resynced
          return $ object $ fields ++
            [ "applied"   .= False
            , "edits"     .= ([] :: [Value])
            , "solutions" .=
                [ trim $ drop 1 $ dropWhile (/= '.') l | l <- drop 1 (lines m) ]
            , "text"      .= m
            ]
        [] -> throwError $ agentError NotApplicable $
          if List.null messages then "proof search found no solution"
          else List.intercalate "\n" messages
    else do
      edits <- forM given $ \ (ii, s) -> do
        hole <- holeRange goals ii
        (,) ii <$> planEdit snap hole (TL.pack s)
      mutationResult env req resynced snap
        ([ "auto ?" ++ show (interactionId ii) ++ " = " ++ TL.unpack (edNew ed)
         | (ii, ed) <- edits ] ++ messages)
        [] (map snd edits)

  -- Case splitting replaces the whole clause the goal is in, not just
  -- the goal: Agda reports the clause's range along with the new
  -- clauses, so no scanning of the source is needed.
  "case" -> do
    (ref, vars) <- withParams req $ \ o -> (,)
      <$> o .: "goal" <*> o .:? "vars" .!= ""
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    hole <- readTCM $ getInteractionRange ii
    snap <- liftIOSafe "reading the source" $ takeSnapshot file
    rs   <- runCommandOK env req (filePath file) $ Cmd_make_case ii noRange vars
    (variant, clause, cs) <-
      case [ (v, r, cs) | Resp_MakeCase ii' v r cs <- rs, ii' == ii ] of
        x : _ -> return x
        []    -> missingInfo "the new clauses"
    let extLam = case variant of
          R.ExtendedLambda -> True
          R.Function       -> False
    ed <- planClause snap clause hole extLam cs
    mutationResult env req resynced snap
      (("case ?" ++ show (interactionId ii) ++ ":") : map ("  " ++) cs)
      [ "goal"    .= object [ "id" .= interactionId ii, "range" .= agentRange hole ]
      , "variant" .= (if extLam then "extendedLambda" else "function" :: Text)
      , "clauses" .= cs
      ]
      [ ed ]

  -- Type-check several candidates against one goal in a single
  -- request.  Each check runs in a state of its own, so the goal
  -- survives all of them, and a rejected candidate is part of the
  -- answer rather than a failed request.
  "checkCandidates" -> do
    (ref, exprs, norm) <- withParams req $ \ o -> (,,)
      <$> o .: "goal" <*> o .: "exprs" <*> normParam Simplified o
    when (List.null exprs) $ throwError $
      agentError InvalidParams "exprs must not be empty"
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    cands <- forM exprs $ \ expr -> do
      out <- runCommand env req (filePath file) $
               Cmd_goal_type_context_check norm ii noRange expr
      case outAborted out of
        Just code -> throwError $ abortError code out
        Nothing -> case responseError (outResponses out) of
          Just err -> (,) expr . Left <$> readTCM (infoErrorDiagnostic err)
          Nothing  -> do
            e <- expectInfo (elaborationIn ii) "an elaborated term" (outResponses out)
            (,) expr . Right <$>
              readTCM (withInteractionId ii $ render <$> prettyATop e)
    fields <- sessionFields env resynced
    gf     <- goalFields ii
    return $ object $ fields ++ gf ++
      [ "candidates" .= map candidateValue cands
      , "text"       .= candidatesText cands
      ]

  "helperType" -> do
    (ref, expr, norm) <- withParams req $ \ o -> (,,)
      <$> o .: "goal" <*> o .: "expr" <*> normParam Simplified o
    resynced <- resync env req
    file <- currentFile env
    ii   <- resolveGoal ref
    rs   <- runCommandOK env req (filePath file) $
              Cmd_helper_function norm ii noRange expr
    oc   <- expectInfo (helperIn ii) "a helper function type" rs
    d      <- readTCM $ withInteractionId ii $ inTopContext $
                render <$> prettyATop oc
    fields <- sessionFields env resynced
    gf     <- goalFields ii
    return $ object $ fields ++ gf ++ [ "signature" .= d, "text" .= d ]

  -- Note that this searches types, not names: it takes space separated
  -- identifiers and lists the names in scope whose type mentions all of
  -- them.
  "search" -> do
    (names, norm) <- withParams req $ \ o -> (,)
      <$> o .: "names" <*> normParam Simplified o
    when (List.null $ trim names) $ throwError $ agentError InvalidParams
      "names must not be empty; search takes space separated identifiers"
    resynced <- resync env req
    file <- currentFile env
    rs   <- runCommandOK env req (filePath file) $
              Cmd_search_about_toplevel norm names
    (hits, searched) <- expectInfo searchResults "the search results" rs
    entries <- readTCM $ mapM prettyNamedType hits
    fields  <- sessionFields env resynced
    return $ object $ fields ++
      [ "searched" .= words searched
      , "results"  .= namedTypesValue entries
      , "text"     .= render (prettySearchAbout searched entries)
      ]

  -- The complement of `search`: it matches names rather than types, and
  -- it looks in every module the project could write an import for
  -- rather than in the scope of the loaded file.
  --
  -- The hits come from an index that a separate process builds beside
  -- the interface files, so until one exists there are none.  What the
  -- answer always carries is the coverage report: which modules an
  -- index would have to cover, which of them have moved since it was
  -- last built, and which have never been checked here and so cannot be
  -- in it.  That costs a stat and 24 bytes per module, and it is what
  -- keeps an unbuilt index from looking like a library with nothing in
  -- it.
  "findNames" -> do
    q <- withParams req nameQueryParams
    when (T.null $ T.strip $ nqPattern q) $ throwError $ agentError InvalidParams
      "pattern must not be empty"
    when (nqLimit q <= 0) $ throwError $ agentError InvalidParams
      "limit must be positive"
    dir  <- projectDir env
    cov  <- readTCM $ scanCoverageIn dir (nqModule q)
    -- The index answers, if there is one; the scan says what the answer
    -- covered either way.
    result <- case (covLocation cov, covIndex cov) of
      (Just loc, Right header) ->
        liftIOSafe "reading the name index" (queryIndex loc header q) >>= \case
          Right r  -> return $ Just r
          Left err -> throwError $ agentError InternalError $ indexErrorText err
      _ -> return Nothing
    let implicit = fromMaybe False (reqShowImplicit req)
        r        = fromMaybe (QueryResult [] 0) result
    fields <- sessionFields env noResync
    return $ object $ fields ++
      [ "pattern" .= nqPattern q
      , "module"  .= nqModule q
      , "glob"    .= nqGlob q
      , "type"    .= nqType q
      , "kinds"   .= nqKinds q
      , "limit"   .= nqLimit q
      , "hits"    .= hitsValue implicit (qrHits r)
      , "matched" .= qrMatched r
      , "index"   .= coverageValue cov
      , "text"    .= List.intercalate "\n"
          (hitsText q implicit r ++ coverageText cov)
      ]

  -- Building the index is the one operation here that costs tens of
  -- seconds, so it is a method an agent calls rather than something a
  -- query does on its own initiative.  It runs in a process of its own
  -- -- loading a whole library peaks at a gigabyte, and this session's
  -- job is to keep one file's proof state responsive -- and that
  -- process's progress is relayed through the ordinary progress events.
  "buildIndex" -> do
    (force, scope) <- withParams req $ \ o -> (,)
      <$> o .:? "force" .!= False
      <*> o .:? "module"
    dir   <- projectDir env
    root  <- readTCM (nameIndexProjectRoot dir) >>= \case
      Just r  -> return r
      Nothing -> throwError $ agentError NotApplicable $ concat
        [ "this project has no .agda-lib file, so there is no build "
        , "directory to hold an index" ]
    -- The decision is about the whole index, since that is what the
    -- builder refreshes; `module` scopes only the report that comes
    -- back, as it scopes a findNames answer.
    whole  <- readTCM $ scanCoverageIn dir Nothing
    before <- if isNothing scope then return whole else
                readTCM $ scanCoverageIn dir scope
    let fresh = case covIndex whole of
          Right{} -> coverageStale whole == 0
          Left{}  -> False
    if fresh && not force then do
      fields <- sessionFields env noResync
      return $ object $ fields ++
        [ "built"     .= False
        , "forced"    .= force
        , "module"    .= scope
        , "index"     .= coverageValue before
        , "text"      .= List.intercalate "\n"
            ( "buildIndex: the index is up to date; pass force to rebuild it"
            : coverageText before )
        ]
    else do
      code  <- runBuilder env req root force
      after <- readTCM $ scanCoverageIn dir scope
      fields <- sessionFields env noResync
      -- How much the build did is in the progress events, which carry
      -- the builder's own count of what it re-read; what the answer
      -- carries is the coverage before and after, which is what the
      -- caller decides on.
      return $ object $ fields ++
        [ "built"     .= True
        , "forced"    .= force
        , "module"    .= scope
        , "exitCode"  .= code
        , "index"     .= coverageValue after
        , "text"      .= List.intercalate "\n"
            ( (if code == 0 then "buildIndex: built"
               else "buildIndex: the builder exited with status " ++ show code)
            : coverageText after )
        ]

  "moduleContents" -> do
    (mref, modName, norm) <- withParams req $ \ o -> (,,)
      <$> o .:? "goal" <*> o .: "module" <*> normParam Simplified o
    resynced <- resync env req
    file     <- currentFile env
    fields   <- sessionFields env resynced
    (rs, scoped) <- case mref of
      Nothing  -> (,) <$> runCommandOK env req (filePath file)
                        (Cmd_show_module_contents_toplevel norm modName)
                      <*> pure Nothing
      Just ref -> do
        ii <- resolveGoal ref
        rs <- runCommandOK env req (filePath file) $
                Cmd_show_module_contents norm ii noRange modName
        return (rs, Just ii)
    (mods, tel, types) <- expectInfo moduleContents "the module contents" rs
    entries <- readTCM $ inGoal scoped $ addContext tel $
                 mapM prettyNamedType types
    gf <- maybe (return []) goalFields scoped
    return $ object $ fields ++ gf ++
      [ "scope"   .= (if isNothing scoped then "topLevel" else "goal" :: Text)
      , "module"  .= modName
      , "modules" .= map prettyShow mods
      , "names"   .= namedTypesValue entries
      , "text"    .= render (prettyModuleContents mods entries)
      ]

  "whyInScope" -> do
    (mref, name) <- withParams req $ \ o -> (,)
      <$> o .:? "goal" <*> o .: "name"
    resynced <- resync env req
    file     <- currentFile env
    fields   <- sessionFields env resynced
    (rs, scoped) <- case mref of
      Nothing  -> (,) <$> runCommandOK env req (filePath file)
                        (Cmd_why_in_scope_toplevel name)
                      <*> pure Nothing
      Just ref -> do
        ii <- resolveGoal ref
        rs <- runCommandOK env req (filePath file) $
                Cmd_why_in_scope ii noRange name
        return (rs, Just ii)
    why <- expectInfo whyInScopeOf "the scope information" rs
    doc <- readTCM $ inGoal scoped $ explainWhyInScope why
    gf  <- maybe (return []) goalFields scoped
    let WhyInScopeData y _ local defs mods = why
    return $ object $ fields ++ gf ++
      [ "scope"       .= (if isNothing scoped then "topLevel" else "goal" :: Text)
      , "name"        .= prettyShow y
      , "local"       .= isJust local
      , "definitions" .=
          [ object [ "name"  .= prettyShow (anameName a)
                   , "kind"  .= show (anameKind a)
                   , "range" .= agentRange (getRange (anameName a)) ]
          | a <- defs ]
      , "modules"     .=
          [ object [ "name"  .= prettyShow (amodName m)
                   , "range" .= agentRange (getRange (amodName m)) ]
          | m <- mods ]
      , "text"        .= render doc
      ]

  -- Where the name at a source position is defined.  This is the one
  -- query that reads the highlighting index rather than running an
  -- interaction; the index is the interface's, so it describes the
  -- file as it was last checked.
  "definitionAt" -> do
    SourcePosition line col <- withParams req $ \ o -> o .: "position"
    resynced <- resync env req
    file <- currentFile env
    snap <- liftIOSafe "reading the source" $ takeSnapshot file
    off  <- case offsetOfLineCol (snapText snap) line col of
      Just n  -> return n
      Nothing -> throwError $ agentError InvalidParams $ concat
        [ "there is no line ", show line, ", column ", show col
        , " in ", filePath file ]
    cf <- lift (gets theCurrentFile) >>= \case
      Just cf -> return cf
      Nothing -> throwError $ agentError NoFileLoaded
        "the file has not been checked yet"
    (mhi, m2s) <- readTCM $ (,)
      <$> (fmap (iHighlighting . miInterface) <$>
             getVisitedModule (currentFileModule cf))
      <*> useSession lensModuleToSource
    hi <- case mhi of
      Just hi -> return hi
      Nothing -> throwError $ agentError InternalError
        "the file has no highlighting information"
    fields <- sessionFields env resynced
    let hit  = aspectsAt hi (fromIntegral off + 1)
        occ  = do
          (r, as) <- hit
          return ( sourceText snap (HRange.from r) (HRange.to r)
                 , as
                 , rangeInFile file snap (HRange.from r) (HRange.to r) )
    site <- case definitionSite . (\ (_, as, _) -> as) =<< occ of
      Nothing -> return Nothing
      Just ds -> Just <$> definitionSiteValue file snap m2s ds
    return $ object $ fields ++
      [ "position"   .= object
          [ "line" .= line, "col" .= col, "offset" .= off ]
      , "symbol"     .= case occ of
          Nothing -> Null
          Just (name, as, r) -> object
            [ "text"  .= name
            , "kind"  .= aspectName (aspect as)
            , "note"  .= (if List.null (note as) then Null else toJSON (note as))
            , "range" .= r
            ]
      , "site"       .= maybe Null fst site
      , "text"       .= List.intercalate "\n" (concat
          [ [ case occ of
                Nothing -> concat
                  [ "nothing at line ", show line, ", column ", show col ]
                Just (name, as, _) ->
                  name ++ " (" ++ T.unpack (aspectName (aspect as)) ++ ")" ]
          , [ snd s | Just s <- [site] ]
          , [ "no definition site"
            | isJust occ, isNothing site ]
          ])
      ]

  "constraints" -> do
    norm     <- withParams req $ normParam AsIs
    resynced <- resync env req
    file     <- currentFile env
    rs       <- runCommandOK env req (filePath file) $ Cmd_constraints norm
    cs       <- expectInfo constraintsOf "the constraints" rs
    docs   <- readTCM $ mapM (fmap render . prettyTCM) cs
    fields <- sessionFields env resynced
    return $ object $ fields ++
      [ "constraints" .=
          [ object [ "text" .= d, "range" .= agentRange r ]
          | (d, OutputForm r _ _ _) <- zip docs cs ]
      , "text" .= (if null docs then "no constraints" else List.intercalate "\n" docs)
      ]

  m -> throwError $ agentError UnknownMethod $ concat
    [ "unknown method ", show m, "; known methods: "
    , List.intercalate ", " (map T.unpack agentMethods)
    ]

  where
  -- Response patterns.
  contextOf ii = \case
    Info_Context ii' ctx | ii == ii' -> Just ctx
    _ -> Nothing
  inferredInGoal ii = \case
    Info_GoalSpecific ii' (Goal_InferredType e) | ii == ii' -> Just e
    _ -> Nothing
  inferredAtTop = \case
    Info_InferredType st _ e -> Just (st, e)
    _ -> Nothing
  normalFormInGoal ii = \case
    Info_GoalSpecific ii' (Goal_NormalForm _ e) | ii == ii' -> Just e
    _ -> Nothing
  normalFormAtTop = \case
    Info_NormalForm st _ _ e -> Just (st, e)
    _ -> Nothing
  constraintsOf = \case
    Info_Constraints cs -> Just cs
    _ -> Nothing
  elaborationIn ii = \case
    Info_GoalSpecific ii' (Goal_GoalType _ (GoalAndElaboration e) _ _ _)
      | ii == ii' -> Just e
    _ -> Nothing
  helperIn ii = \case
    Info_GoalSpecific ii' (Goal_HelperFunction oc) | ii == ii' -> Just oc
    _ -> Nothing
  searchResults = \case
    Info_SearchAbout hits names -> Just (hits, names)
    _ -> Nothing
  moduleContents = \case
    Info_ModuleContents mods tel types -> Just (mods, tel, types)
    _ -> Nothing
  whyInScopeOf = \case
    Info_WhyInScope why -> Just why
    _ -> Nothing
  -- Answers about a goal are printed in its scope.
  inGoal :: Maybe InteractionId -> TCM a -> TCM a
  inGoal = maybe id withInteractionId
  -- 'Cmd_autoOne' announces a list of candidate solutions like this.
  solutionsHeader = "Solutions:"

-- | The result of a load or reload.
loadResult :: AgentEnv -> [Response] -> Resync -> AgentM Value
loadResult env rs resynced = do
  (ms, wes) <- expectInfo allGoalsInfo "the goal list" rs
  fields <- sessionFields env resynced
  gs     <- encodeGoals ms wes
  return $ object $ fields ++ gs

-- | The result of a command that displays a goal type and context.
goalTypeResult :: AgentEnv -> Resync -> InteractionId -> [Response] -> AgentM Value
goalTypeResult env resynced ii rs = do
  info <- expectInfo goalType "the goal type" rs
  fields  <- sessionFields env resynced
  gf      <- goalFields ii
  payload <- case info of
    (norm, aux, ctx, bndry, constraints) ->
      encodeGoalType ii norm aux ctx bndry constraints
  return $ object $ fields ++ gf ++ payload
  where
  goalType = \case
    Info_GoalSpecific ii' (Goal_GoalType norm aux ctx bndry cs)
      | ii == ii' -> Just (norm, aux, ctx, bndry, cs)
    _ -> Nothing

-- | Add fields to an object, overriding fields of the same name.
mergeObject :: Value -> [Pair] -> Value
mergeObject (Object o) ps = Object $ case object ps of
  Object o' -> o' <> o
  _         -> o
mergeObject v _ = v

-- | The rendered text of a result, if it has one.
resultText :: Value -> Maybe String
resultText (Object o) = case KeyMap.lookup "text" o of
  Just (String t) -> Just $ T.unpack t
  _               -> Nothing
resultText _ = Nothing
