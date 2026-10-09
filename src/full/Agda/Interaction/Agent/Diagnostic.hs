{-# OPTIONS_GHC -Wunused-imports #-}

-- | Structured diagnostics for the agent frontend.
--
--   Agda renders an error as one block of text with the range and the
--   error's name baked into it.  That reads well, and the agent
--   frontend keeps it as the message; but a client that wants to jump
--   to the problem, or to decide what to do about it, needs the pieces:
--
--   * @code@ — Agda's own stable name for the error or warning, such as
--     @UnequalTypes@ or @UnsolvedMetaVariables@;
--   * @range@ — where it is, rather than a file:line prefix in prose;
--   * @related@ — the calls the type checker was in when it raised the
--     error (the trailing \"when checking …\" of the rendering), each
--     with its own range.
--
--   The same module carries the structured form of a context entry,
--   which is where a goal's modalities become visible to a client.

module Agda.Interaction.Agent.Diagnostic
  ( -- * Diagnostics
    Severity(..)
  , Diagnostic(..)
  , Related(..)
  , diagnosticValue
  , errorDiagnostic
  , warningDiagnostic
  , nonFatalErrorDiagnostic
  , infoErrorDiagnostic
    -- * Context entries
  , contextEntryValue
  ) where

import Control.DeepSeq        ( force )
import Control.Exception      ( evaluate )
import Control.Monad          ( forM )
import Control.Monad.Except   ( MonadError(catchError) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.Aeson
import Data.Set qualified as Set
import Data.Text             ( Text )
import Data.Text qualified as T

import Agda.Interaction.Agent.Protocol ( AgentRange, agentRange )
import Agda.Interaction.EmacsTop       ( prettyInfoError )
import Agda.Interaction.Options.Warnings ( warningNameToString )
import Agda.Interaction.Response       ( Info_Error, Info_Error_boot(..)
                                       , ResponseContextEntry(..) )

import Agda.Syntax.Abstract.Pretty     ( prettyATop )
import Agda.Syntax.Common
  ( Arg(..), Cohesion(..), Hiding(..), Quantity(..), Relevance(..)
  , getCohesion, getHiding, getQuantity, getRelevance )
import Agda.Syntax.Common.Pretty       ( pretty, prettyShow, render )
import Agda.Syntax.Concrete.Name       ( NameInScope(..), isInScope )
import Agda.Syntax.Position            ( getRange, noRange )

import Agda.TypeChecking.Errors        ( prettyError )
import Agda.TypeChecking.Errors.Names  ( typeErrorString )
import Agda.TypeChecking.Monad hiding ( (.=) )
import Agda.TypeChecking.Pretty        ( prettyTCM )

import Agda.Utils.Impossible           ( CatchImpossible(catchImpossible) )

------------------------------------------------------------------------
-- * Diagnostics

data Severity = SevError | SevWarning

severityName :: Severity -> Text
severityName = \case
  SevError   -> "error"
  SevWarning -> "warning"

-- | Something Agda has to say about a place in the source.
data Diagnostic = Diagnostic
  { diagSeverity :: Severity
  , diagCode     :: Maybe Text
      -- ^ Agda's own name for it, where it has one.
  , diagMessage  :: String
      -- ^ Agda's rendering, unchanged.
  , diagRange    :: Maybe AgentRange
  , diagRelated  :: [Related]
  }

-- | A place the message refers to.
data Related = Related
  { relMessage :: String
  , relRange   :: Maybe AgentRange
  }

instance ToJSON Related where
  toJSON r = object [ "message" .= relMessage r, "range" .= relRange r ]

diagnosticValue :: Diagnostic -> Value
diagnosticValue d = object $
  [ "severity" .= severityName (diagSeverity d)
  , "message"  .= diagMessage d
  , "range"    .= diagRange d
  ] ++
  [ "code"     .= c | Just c <- [ diagCode d ] ] ++
  [ "related"  .= diagRelated d | not (null (diagRelated d)) ]

-- | How many enclosing calls to report.  The chain can be long, and
--   the ones near the error are the informative ones.
relatedLimit :: Int
relatedLimit = 8

-- | An error, as Agda renders it, plus its name, its range and the
--   calls it was raised under.
errorDiagnostic :: TCErr -> TCM Diagnostic
errorDiagnostic err = do
  doc <- prettyError err
  rel <- case err of
    TypeError _ s e -> withTCState (const s) $ relatedCalls $ clEnv e ^. eCall
    _               -> return []
  return Diagnostic
    { diagSeverity = SevError
    , diagCode     = case err of
        TypeError _ _ e -> Just $ T.pack $ typeErrorString $ clValue e
        ParserError{}   -> Just "SyntaxError"
        IOException{}   -> Just "IOError"
        _               -> Nothing
    , diagMessage  = render doc
    , diagRange    = agentRange $ getRange err
    , diagRelated  = rel
    }

-- | The calls the type checker was in: the trailing \"when checking …\"
--   of a rendered error, as ranges rather than as prose.
relatedCalls :: Maybe (Closure Call) -> TCM [Related]
relatedCalls = go relatedLimit
  where
  go _ Nothing  = return []
  go 0 _        = return []
  go n (Just c) = (:) <$> relatedCall c <*> go (n - 1) (clEnv c ^. eCall)

-- | One enclosing call, rendered as far as it can be.
--
--   A related call is context around the diagnostic, so a call that
--   will not print must not take the diagnostic with it.  Agda has
--   calls that carry deliberate '__IMPOSSIBLE__' placeholders — the
--   synthetic declaration that a pattern lambda, or a let-bound
--   function, is scope checked as
--   ('Agda.Syntax.Translation.ConcreteToAbstract') — and an
--   '__IMPOSSIBLE__' is a plain Haskell exception rather than a
--   'TCErr', so nothing else here would catch it: printing one used to
--   take the whole Agda process down, and with it every account of the
--   error that was actually being reported.  Such a frame keeps its
--   range, which is what says /which/ lambda it was, and says in place
--   of the message why it has none.
relatedCall :: Closure Call -> TCM Related
relatedCall c = enterClosure c $ \ call -> do
  msg <- guarded $ do
    doc <- prettyTCM call
    liftIO $ evaluate $ force $ render doc
  rng <- guarded $ liftIO $ evaluate $ force $ getRange call
  return Related
    { relMessage = either (unprintable call) id msg
    , relRange   = agentRange $ either (const noRange) id rng
    }
  where
  -- Run a piece of rendering, reporting rather than propagating a
  -- failure.  The rendering is forced inside, since both an
  -- 'Impossible' and a 'TCErr' can be buried in a lazily built
  -- document.
  guarded :: TCM a -> TCM (Either String a)
  guarded m =
    ((Right <$> m) `catchImpossible` (return . Left . oneLine . show))
      `catchError` \ _ -> return $ Left "the printer raised an error"

  unprintable call reason =
    "(" ++ render (pretty call) ++ " could not be printed: " ++ reason ++ ")"

  oneLine = unwords . words

-- | A warning, with the name it is switched on and off by.
warningDiagnostic :: TCWarning -> TCM Diagnostic
warningDiagnostic = tcWarningDiagnostic SevWarning

-- | A non-fatal error.  Agda carries these as warnings, but they are
--   errors, and a client acts on them differently.
nonFatalErrorDiagnostic :: TCWarning -> TCM Diagnostic
nonFatalErrorDiagnostic = tcWarningDiagnostic SevError

tcWarningDiagnostic :: Severity -> TCWarning -> TCM Diagnostic
tcWarningDiagnostic sev w = return Diagnostic
  { diagSeverity = sev
  , diagCode     = Just $ T.pack $ warningNameToString $ warningName $ tcWarning w
  , diagMessage  = render $ tcWarningDoc w
  , diagRange    = agentRange $ tcWarningRange w
  , diagRelated  = []
  }

-- | The error of a failed interactive command.
infoErrorDiagnostic :: Info_Error -> TCM Diagnostic
infoErrorDiagnostic = \case
  Info_GenericError err -> errorDiagnostic err
  info -> do
    doc <- prettyInfoError info
    rel <- case info of
      Info_CompilationError ws -> forM (Set.toAscList ws) $ \ w -> return Related
        { relMessage = render $ tcWarningDoc w
        , relRange   = agentRange $ tcWarningRange w
        }
      _ -> return []
    return Diagnostic
      { diagSeverity = SevError
      , diagCode     = case info of
          Info_CompilationError{}            -> Just "CompilationError"
          Info_HighlightingParseError{}      -> Just "HighlightingParseError"
          Info_HighlightingScopeCheckError{} -> Just "HighlightingScopeCheckError"
      , diagMessage  = render doc
      , diagRange    = Nothing
      , diagRelated  = rel
      }

------------------------------------------------------------------------
-- * Context entries

-- | One entry of a goal's context.
--
--   Besides the name, type and let value, this reports what the Emacs
--   mode shows through decoration alone: whether the entry is in scope
--   under the name given, and its modality.
contextEntryValue :: ResponseContextEntry -> TCM Value
contextEntryValue (ResponseContextEntry n x (Arg info e) letv nis) = do
  ty  <- render <$> prettyATop e
  val <- traverse (fmap render . prettyATop) letv
  return $ object
    [ "name"         .= prettyShow x
    , "originalName" .= prettyShow n
    , "type"         .= ty
    , "value"        .= val
    , "inScope"      .= (isInScope nis == InScope)
    , "modality"     .= object
        [ "hiding"    .= hidingName    (getHiding info)
        , "relevance" .= relevanceName (getRelevance info)
        , "quantity"  .= quantityName  (getQuantity info)
        , "cohesion"  .= cohesionName  (getCohesion info)
        ]
    ]

-- The names below are the agent protocol's, chosen to be stable: Agda's
-- own 'Pretty' instances render surface syntax, which depends on how
-- the modality was written down.

hidingName :: Hiding -> Text
hidingName = \case
  Hidden     -> "hidden"
  Instance{} -> "instance"
  NotHidden  -> "visible"

relevanceName :: Relevance -> Text
relevanceName = \case
  Relevant{}        -> "relevant"
  Irrelevant{}      -> "irrelevant"
  ShapeIrrelevant{} -> "shapeIrrelevant"

quantityName :: Quantity -> Text
quantityName = \case
  Quantity0{} -> "erased"
  Quantity1{} -> "linear"
  Quantityω{} -> "unrestricted"

cohesionName :: Cohesion -> Text
cohesionName = \case
  Flat       -> "flat"
  Continuous -> "continuous"
  Sharp      -> "sharp"
  Squash     -> "squash"
