{-# OPTIONS_GHC -Wunused-imports #-}

-- | Wire format of the agent interaction frontend (@agda --interaction-agent@).
--
--   The frontend speaks JSON lines over stdio.  A client sends
--
--   > {"id": <any>, "method": <string>, "params": {...},
--   >  "timeoutMs": <int>?, "expectRevision": <int>?}
--
--   and receives zero or more event lines
--
--   > {"id": <any>, "event": <string>, ...}
--
--   followed by exactly one terminal line, either
--
--   > {"id": <any>, "result": {...}}
--
--   or
--
--   > {"id": <any>, "error": {"code": ..., "message": ..., "range": ..., "data": ...}}
--
--   Before the first request the frontend emits an unsolicited
--   @{"event":"ready","handshake":{...}}@ line.
--
--   This module only describes the envelope, positions and errors; the
--   payload of each method is assembled in "Agda.Interaction.Agent.Top".

module Agda.Interaction.Agent.Protocol
  ( -- * Version
    agentProtocolVersion
  , agentMethods
  , handshakeValue
    -- * Requests
  , AgentRequest(..)
  , GoalRef(..)
  , SourcePosition(..)
  , parseRewrite
  , parseComputeMode
  , rewriteName
  , computeModeName
    -- * Positions
  , AgentPosition(..)
  , AgentRange(..)
  , agentPosition
  , agentRange
    -- * Errors
  , AgentErrorCode(..)
  , agentErrorCodeName
  , AgentError(..)
  , agentError
  , agentErrorAt
  , agentErrorData
    -- * Replies
  , resultLine
  , errorLine
  , eventLine
  ) where

import Data.Aeson
import Data.Aeson.Types  ( Pair, Parser, typeMismatch )
import Data.Char         ( toLower, isAlpha )
import Data.Text         ( Text )
import Data.Text qualified as T
import Data.Word         ( Word32 )

import Agda.Interaction.Base ( ComputeMode(..), Rewrite(..) )

import Agda.Syntax.Position
  ( Position'(..), Range, posCol, posLine, posPos, rEnd, rStart, rangeFile
  , rangeFilePath )

import Agda.Utils.FileName        ( filePath )
import Agda.Utils.Maybe.Strict qualified as Strict

import Agda.VersionCommit ( versionWithCommitInfo )

------------------------------------------------------------------------
-- * Version and handshake

-- | The version of the agent protocol implemented here.
--
--   Bump this whenever the meaning of an existing field changes; adding
--   fields or methods does not require a bump.
agentProtocolVersion :: Int
agentProtocolVersion = 2

-- | The methods understood by this version of the frontend.
agentMethods :: [Text]
agentMethods =
  [ "handshake", "status"
  , "load", "reload"
  , "goals", "goal", "context", "check", "checkCandidates"
  , "infer", "normalise", "constraints"
  , "helperType", "search", "findNames", "buildIndex", "moduleContents"
  , "whyInScope"
  , "definitionAt"
  , "give", "elaborate", "refine", "intro", "case", "solve", "auto"
  , "abort", "close"
  ]

-- | The payload of the @ready@ event and of the @handshake@ method.
handshakeValue :: Value
handshakeValue = object
  [ "protocolVersion"    .= agentProtocolVersion
  , "agdaVersion"        .= (versionWithCommitInfo :: String)
  , "methods"            .= agentMethods
  , "normalisationModes" .= map rewriteName
      [ AsIs, Instantiated, HeadNormal, Simplified, Normalised ]
  , "computeModes"       .= map computeModeName
      [ DefaultCompute, HeadCompute, IgnoreAbstract, UseShowInstance ]
  , "positionEncoding"   .= object
      [ "unit"             .= ("codePoint" :: Text)
      , "lineBase"         .= (1 :: Int)
      , "columnBase"       .= (1 :: Int)
      , "offsetBase"       .= (0 :: Int)
      , "rangeConvention"  .= ("half-open" :: Text)
      ]
  , "defaults"           .= object
      [ "goals"       .= rewriteName AsIs
      , "goal"        .= rewriteName Simplified
      , "context"     .= rewriteName Simplified
      , "check"       .= rewriteName Simplified
      , "checkCandidates" .= rewriteName Simplified
      , "infer"       .= rewriteName Simplified
      , "helperType"  .= rewriteName Simplified
      , "search"      .= rewriteName Simplified
      , "moduleContents" .= rewriteName Simplified
      , "constraints" .= rewriteName AsIs
      , "normalise"   .= computeModeName DefaultCompute
      , "elaborate"   .= rewriteName Normalised
      , "solve"       .= rewriteName Instantiated
      , "auto"        .= rewriteName AsIs
      ]
  , "capabilities"       .= object
        -- Queries transparently reload a file that changed on disk.
      [ "autoResync"            .= True
        -- A query against a file unchanged since a failed load answers
        -- with that load's error, without checking the file again.
      , "cachedFailure"         .= True
      , "timeouts"              .= True
        -- load and reload take `profile`; a timeout names the
        -- definition it was checking.
      , "profiling"             .= True
        -- The methods that rewrite the source.
      , "mutations"             .= True
      , "caseSplit"             .= True
        -- Errors and warnings carry Agda's own name for them, a range,
        -- and the calls they were raised under.
      , "structuredDiagnostics" .= True
        -- showImplicit and showIrrelevant in the request envelope.
      , "displayOptions"        .= True
        -- Where the name at a source position is defined.
      , "definitionAt"          .= True
        -- The findNames method and the coverage report it carries.
      , "nameIndex"             .= True
      ]
  , "notes"              .= object
      [ "topLevelScope" .=
          ("Top-level infer/normalise print names qualified w.r.t. the top-level scope."
            :: Text)
      , "mutations" .=
          ("A mutating method rewrites the file on disk and reloads it, so the goal \
           \identifiers in its result are the ones of the new revision.  Its \
           \`edits` field lists the replacements that were made."
            :: Text)
      , "profiling" .=
          ("`load` and `reload` take `profile`: a profiling option such as \
           \\"definitions\" (or `true` for that one), or a list of them.  The \
           \file is then checked from source even if its interface is current, \
           \and the result's `profile` lists the CPU time billed to each \
           \definition, most expensive first.  A request that times out while \
           \checking a definition names it in `data.checking`, and \
           \`data.profile` holds the timings up to then."
            :: Text)
      , "search" .=
          ("The `search` method searches types, not names: it takes space \
           \separated identifiers and lists the names in scope whose type \
           \mentions all of them.  A name that merely contains one of the \
           \identifiers is not a hit, and a module the loaded file does not \
           \import is not searched.  To look for a name by its spelling, or \
           \to look outside what the file imports, use `findNames`."
            :: Text)
      , "findNames" .=
          ("The `findNames` method searches names, not types, and it searches \
           \every module the project could import rather than the scope of the \
           \loaded file; `search` is the complementary query.  It answers from \
           \an index built beside the interface files, so its `hits` are empty \
           \until `buildIndex` has made one.  Hits are ranked: 1 the whole \
           \name, 2 a prefix of it, 3 a whole hyphen- or underscore-separated \
           \component, 4 a substring of it, and -- only when `type` is given \
           \-- 5 a name the type mentions; ties break on `uses`, then on where \
           \the hit lives, the project's own modules before its libraries' and \
           \Agda's builtins last.  The `index` \
           \object every answer carries says what the answer covered: \
           \`builtAt` is null when there is no index, `stale` counts checked \
           \modules the index does not match, `unindexed` counts modules \
           \that have never been checked here and so are not in it until a build \
           \checks them, and \
           \`modified` counts modules that have been edited since they were \
           \checked -- the index describes those as they were checked, and \
           \no build changes that until Agda checks them again."
            :: Text)
      , "buildIndex" .=
          ("The `buildIndex` method builds the index `findNames` answers from, \
           \in a process of its own, and relays its output as progress \
           \events.  The first build costs tens of seconds on a large library, \
           \which is why no query spends it unasked; give it a generous \
           \`timeoutMs`.  Afterwards it re-renders only the modules whose \
           \interfaces have moved, so keeping the index current after an edit \
           \costs a second or two, and calling it whenever `stale` is nonzero \
           \is the intended use.  With nothing \
           \stale it does nothing unless `force` is set, which re-renders the \
           \whole index.  `module` scopes the report that comes back, not the \
           \build."
            :: Text)
      , "displayOptions" .=
          ("`showImplicit` and `showIrrelevant` hold for one request only.  They \
           \change what Agda prints, so on a mutating method they change the text \
           \that is written to the file as well."
            :: Text)
      ]
  ]

------------------------------------------------------------------------
-- * Requests

-- | One decoded request line.
data AgentRequest = AgentRequest
  { reqId             :: Value
      -- ^ Echoed verbatim in the terminal line.  'Null' if absent.
  , reqMethod         :: Text
  , reqParams         :: Value
  , reqTimeoutMs      :: Maybe Int
      -- ^ If the request has not finished within this many milliseconds
      --   the abort machinery is triggered and a @timeout@ error returned.
  , reqExpectRevision :: Maybe Int
      -- ^ Opt in to strict revision checking instead of auto-resync.
  , reqShowImplicit   :: Maybe Bool
      -- ^ Print implicit arguments while answering this request.
  , reqShowIrrelevant :: Maybe Bool
      -- ^ Print irrelevant arguments while answering this request.
  }

instance FromJSON AgentRequest where
  parseJSON = withObject "request" $ \ o -> AgentRequest
    <$> o .:? "id" .!= Null
    <*> o .:  "method"
    <*> o .:? "params" .!= object []
    <*> o .:? "timeoutMs"
    <*> o .:? "expectRevision"
    <*> o .:? "showImplicit"
    <*> o .:? "showIrrelevant"

-- | How a request addresses a goal: by interaction id, or by a source
--   position inside the goal's range (stable across renumbering).
data GoalRef
  = GoalById  Int
  | GoalAtPos Word32 Word32  -- ^ Line and column, both 1-based.

instance FromJSON GoalRef where
  parseJSON = \case
    Number n     -> GoalById <$> parseJSON (Number n)
    String s     -> case reads (T.unpack s) of
      [(n, "")] -> pure $ GoalById n
      _         -> fail $ "not a goal id: " ++ T.unpack s
    Object o     -> GoalAtPos <$> o .: "line" <*> o .: "col"
    v            -> typeMismatch "goal id or {line, col}" v

-- | A source position named by a client, as @{"line": L, "col": C}@ or
--   as @"L:C"@.  Both count from 1.
data SourcePosition = SourcePosition !Word32 !Word32

instance FromJSON SourcePosition where
  parseJSON = \case
    Object o -> SourcePosition <$> o .: "line" <*> o .: "col"
    String s | (l, ':' : c) <- break (== ':') (T.unpack s)
             , [(l', "")] <- reads l
             , [(c', "")] <- reads c
             -> pure $ SourcePosition l' c'
    v -> typeMismatch "{line, col} or \"line:col\"" v

-- | Normalisation modes, as spelled by clients.
--
--   Names are matched case-insensitively with non-letters removed, so
--   @head-normal@, @headNormal@ and @HeadNormal@ all work.
parseRewrite :: Text -> Parser Rewrite
parseRewrite t = case normaliseName t of
  "asis"         -> pure AsIs
  "none"         -> pure AsIs
  "instantiated" -> pure Instantiated
  "headnormal"   -> pure HeadNormal
  "simplified"   -> pure Simplified
  "normalised"   -> pure Normalised
  "normalized"   -> pure Normalised
  _              -> fail $ "unknown normalisation mode: " ++ T.unpack t

-- | The name under which a 'Rewrite' is reported.
rewriteName :: Rewrite -> Text
rewriteName = \case
  AsIs         -> "asIs"
  Instantiated -> "instantiated"
  HeadNormal   -> "headNormal"
  Simplified   -> "simplified"
  Normalised   -> "normalised"

-- | Compute modes, as spelled by clients.
parseComputeMode :: Text -> Parser ComputeMode
parseComputeMode t = case normaliseName t of
  "default"        -> pure DefaultCompute
  "normalise"      -> pure DefaultCompute
  "normalize"      -> pure DefaultCompute
  "head"           -> pure HeadCompute
  "headcompute"    -> pure HeadCompute
  "ignoreabstract" -> pure IgnoreAbstract
  "show"           -> pure UseShowInstance
  "useshowinstance"-> pure UseShowInstance
  _                -> fail $ "unknown compute mode: " ++ T.unpack t

-- | The name under which a 'ComputeMode' is reported.
computeModeName :: ComputeMode -> Text
computeModeName = \case
  DefaultCompute  -> "default"
  HeadCompute     -> "head"
  IgnoreAbstract  -> "ignoreAbstract"
  UseShowInstance -> "show"

normaliseName :: Text -> String
normaliseName = map toLower . filter isAlpha . T.unpack

------------------------------------------------------------------------
-- * Positions

-- | A source position.  Lines and columns count from 1, the offset
--   counts Unicode code points from 0.
data AgentPosition = AgentPosition
  { apLine   :: !Word32
  , apCol    :: !Word32
  , apOffset :: !Word32
  }

instance ToJSON AgentPosition where
  toJSON p = object
    [ "line"   .= apLine p
    , "col"    .= apCol p
    , "offset" .= apOffset p
    ]

-- | A half-open source range: @start@ is included, @end@ is not.
data AgentRange = AgentRange
  { arFile  :: Maybe FilePath
  , arStart :: AgentPosition
  , arEnd   :: AgentPosition
  }

instance ToJSON AgentRange where
  toJSON r = object
    [ "file"  .= arFile r
    , "start" .= arStart r
    , "end"   .= arEnd r
    ]

agentPosition :: Position' a -> AgentPosition
agentPosition p = AgentPosition
  { apLine   = posLine p
  , apCol    = posCol p
  , apOffset = posPos p - 1  -- 'posPos' counts from 1.
  }

-- | The smallest range covering the given one, or 'Nothing' for
--   'Agda.Syntax.Position.noRange'.
agentRange :: Range -> Maybe AgentRange
agentRange r = do
  s <- rStart r
  e <- rEnd r
  return AgentRange
    { arFile  = filePath . rangeFilePath <$> Strict.toLazy (rangeFile r)
    , arStart = agentPosition s
    , arEnd   = agentPosition e
    }

------------------------------------------------------------------------
-- * Errors

-- | Machine-readable classification of a failed request.
data AgentErrorCode
  = ParseError
    -- ^ The request line is not JSON.
  | InvalidRequest
    -- ^ The request line is JSON, but not a request.
  | UnknownMethod
  | InvalidParams
  | NoFileLoaded
    -- ^ The method needs a loaded file, and there is none.
  | UnknownGoal
  | NotApplicable
    -- ^ The command ran, but had nothing to do: no introduction form,
    --   no proof found, no goal solved.
  | StaleDocument
    -- ^ @expectRevision@ was given and does not match.
  | TimeoutError
    -- ^ The request exceeded its @timeoutMs@ and was aborted.
  | AbortedError
    -- ^ The request was aborted by an @abort@ request.
  | AgdaError
    -- ^ Agda reported an error; @message@ is the rendered error.
  | AgentIOError
  | InternalError
  deriving (Eq, Show)

agentErrorCodeName :: AgentErrorCode -> Text
agentErrorCodeName = \case
  ParseError     -> "parseError"
  InvalidRequest -> "invalidRequest"
  UnknownMethod  -> "unknownMethod"
  InvalidParams  -> "invalidParams"
  NoFileLoaded   -> "noFileLoaded"
  UnknownGoal    -> "unknownGoal"
  NotApplicable  -> "notApplicable"
  StaleDocument  -> "staleDocument"
  TimeoutError   -> "timeout"
  AbortedError   -> "aborted"
  AgdaError      -> "agdaError"
  AgentIOError   -> "ioError"
  InternalError  -> "internalError"

data AgentError = AgentError
  { aeCode     :: AgentErrorCode
      -- ^ How the request failed, in the protocol's own terms.
  , aeMessage  :: String
  , aeRange    :: Maybe AgentRange
  , aeAgdaCode :: Maybe Text
      -- ^ Agda's own name for the error, e.g. @UnequalTypes@, when the
      --   failure is one of Agda's rather than one of the protocol's.
  , aeRelated  :: [Value]
      -- ^ Other places the message refers to, each @{message, range}@.
  , aeData     :: [Pair]
      -- ^ Extra machine-readable context, e.g. the goals that do exist.
  }

instance ToJSON AgentError where
  toJSON e = object $
    [ "code"     .= agentErrorCodeName (aeCode e)
    , "message"  .= aeMessage e
    ] ++
    [ "agdaCode" .= c | Just c <- [aeAgdaCode e] ] ++
    [ "range"    .= r | Just r <- [aeRange e] ] ++
    [ "related"  .= aeRelated e | not (null (aeRelated e)) ] ++
    [ "data"     .= object (aeData e) | not (null (aeData e)) ]

agentError :: AgentErrorCode -> String -> AgentError
agentError code msg = AgentError code msg Nothing Nothing [] []

agentErrorAt :: AgentErrorCode -> String -> Maybe AgentRange -> AgentError
agentErrorAt code msg r = (agentError code msg){ aeRange = r }

-- | An error carrying machine-readable context, such as the goals that
--   do exist when the one asked for does not.
agentErrorData :: AgentErrorCode -> String -> [Pair] -> AgentError
agentErrorData code msg ps = (agentError code msg){ aeData = ps }

------------------------------------------------------------------------
-- * Replies

-- | The terminal line of a successful request.
resultLine :: Value -> Value -> Value
resultLine i v = object [ "id" .= i, "result" .= v ]

-- | The terminal line of a failed request.
errorLine :: Value -> AgentError -> Value
errorLine i e = object [ "id" .= i, "error" .= e ]

-- | A non-terminal line emitted while a request is running.
eventLine :: Value -> Text -> [Pair] -> Value
eventLine i ev ps = object $ [ "id" .= i, "event" .= ev ] ++ ps
