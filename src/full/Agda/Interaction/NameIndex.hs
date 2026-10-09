{-# OPTIONS_GHC -Wunused-imports #-}

-- | A project-wide index of the names a project could import.
--
--   The index answers one question -- /does this project already define
--   something called roughly this, and if so, what is its type and
--   where is it?/ -- over every module the project could write an
--   @import@ for, rather than over the scope of the loaded file.  See
--   @notes\/name-index.md@ for the design.
--
--   This module holds three things:
--
--   * the on-disk format, with its reader and its writer,
--
--   * the /coverage scan/, which says which modules an index would have
--     to cover, which of them have moved since it was last built, and
--     which have no usable interface at all, and
--
--   * the /builder/, which loads the include path and renders it.
--
--   The scan is what makes an answer honest: it costs a stat and 24
--   bytes per module -- 'readInterfacePrefix' reads the uncompressed
--   prefix of an interface without decompressing its body -- so every
--   query can pay for it, and a query answered over a library that has
--   never been checked says so instead of looking like a library with
--   nothing in it.
--
--   The builder runs as a process of its own (@--build-name-index@),
--   because loading a whole library peaks at about a gigabyte and an
--   interactive session's job is to keep one file's proof state
--   responsive.
--
--   Nothing here is particular to a frontend.  A query needs no type
--   checker -- the builder renders every type to text -- so the file is
--   the product, and the agent frontend, the @agda-client@ client and
--   the Emacs mode read it as peers.

module Agda.Interaction.NameIndex
  ( -- * The on-disk format
    indexFormatVersion
  , indexFileName
  , IndexHeader(..)
  , IndexModuleInfo(..)
  , IndexModule(..)
  , IndexEntry(..)
  , NameIndex(..)
  , nameIndexFile
  , nameIndexFileIn
  , nameIndexProjectRoot
    -- * Reading and writing
  , IndexError(..)
  , indexErrorText
  , readIndexHeader
  , readIndex
  , writeIndex
    -- * The coverage scan
  , NoInterface(..)
  , ModuleState(..)
  , ScannedModule(..)
  , Coverage(..)
  , scanCoverage
  , scanCoverageIn
  , matchesModulePrefix
  , coverageValue
  , coverageText
  , coverageStale
  , nameIndexStatus
    -- * The query
  , NameQuery(..)
  , MatchRank(..)
  , rankNumber
  , rankName
  , Matcher
  , compilePattern
  , rankOf
  , mentionRank
  , nameComponents
  , matchesKind
  , mentionsAll
  , Hit(..)
  , QueryResult(..)
  , queryIndex
  , jsonNeedle
  , hitsValue
  , hitsText
    -- * The builder
  , buildNameIndex
  ) where

import Prelude hiding ( null )

import Control.Exception qualified as E
import Control.Monad          ( forM, forM_, guard, unless, when )
import Control.Monad.Except   ( MonadError(..) )
import Control.Monad.IO.Class ( MonadIO(..) )

import Data.Aeson
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Functor        ( (<&>) )
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe          ( isJust )
import Data.Ord            ( Down(..), comparing )
import Data.Set qualified as Set
import Data.Text              ( Text )
import Data.Text qualified as T
import Data.Time.Clock        ( UTCTime, getCurrentTime )
import Data.Word              ( Word32, Word64 )

import Numeric                ( readHex, showHex )

import System.Directory
  ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory
  , getModificationTime, removeFile, renameFile )
import System.FilePath        ( addTrailingPathSeparator, takeDirectory, (</>) )
import System.IO.Error        ( catchIOError, isDoesNotExistError )

import Agda.Interaction.FindFile ( enumerateAgdaFiles, toIFile )
import Agda.Interaction.Imports qualified as Imp
import Agda.Interaction.Library
  ( pattern AgdaLibFile, findProjectRoot, getAgdaLibFile
  , getPrimitiveLibDir, _libPragmas )
import Agda.Interaction.Options
  ( NameIndexBuild(..), optAbsoluteIncludePaths )
import Agda.Interaction.Options.HasOptions ( commandLineOptions )

import Agda.Syntax.Abstract.Name
  ( AbstractName(..), ModuleName, QName, mnameToList, nameBindingSite
  , nameConcrete, nameId, qnameName )
import Agda.Syntax.Common        ( NameId(..) )
import Agda.Syntax.Common.Pretty
  ( Doc, Mode(OneLineMode), Style(..), prettyShow, renderStyle, style )
import Agda.Syntax.Concrete qualified as C
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Position
  ( Range, iStart, noRange, posCol, posLine, rangeIntervals )
import Agda.Syntax.Scope.Base    ( allNamesInScope )
import Agda.Syntax.TopLevelModuleName
  ( RawTopLevelModuleName(..), TopLevelModuleName, moduleNameId )

import Agda.TypeChecking.Errors        ( prettyError )
import Agda.TypeChecking.Monad.Base
  ( Definition(..), Interface(..), ModuleInfo(..), TCErr(GenericException), TCM
  , getTC, iFullHash, putTC, srcFromPath )
import Agda.TypeChecking.Monad.Context ( addContext )
import Agda.TypeChecking.Monad.Debug   ( alwaysReportSLn )
import Agda.TypeChecking.Monad.Imports ( getDecodedModules, setDecodedModules )
import Agda.TypeChecking.Monad.Options
  ( checkAndSetOptionsFromPragma, libToTCM, withShowAllArguments' )
import Agda.TypeChecking.Monad.Signature ( getConstInfo', lookupSection )
import Agda.TypeChecking.Monad.State
  ( evalWithScope, recomputeInverseScope, topLevelModuleName )
import Agda.TypeChecking.Pretty        ( prettyTCM )
import Agda.TypeChecking.Serialise
  ( currentInterfaceVersion, readInterfacePrefix )
import Agda.TypeChecking.Substitute    ( TelV(..) )
import Agda.TypeChecking.Telescope     ( telViewUpTo )

import Agda.Utils.FileName    ( AbsolutePath, filePath, mkAbsolute )
import Agda.Utils.Hash        ( Hash, hashTextFile )
import Agda.Utils.Impossible  ( CatchImpossible(catchImpossible) )
import Agda.Utils.List        ( nubOn )
import Agda.Utils.List1 qualified as List1
import Agda.Utils.Null        ( null )
import Agda.Utils.Size        ( size )

import Agda.Version           ( version )

------------------------------------------------------------------------
-- * The on-disk format

-- | Bumped whenever the shape of the index file changes.  An index of a
--   different format is treated as no index at all.
indexFormatVersion :: Int
indexFormatVersion = 3

-- | The name the index goes by inside the build directory.
indexFileName :: FilePath
indexFileName = "name-index.jsonl"

-- | Where a project's index lives: under @_build\/\<version\>\/agda@,
--   beside the interfaces it is derived from, so that whatever
--   invalidates those invalidates it and it never appears in the source
--   tree.
--
--   'Nothing' for a project with no @.agda-lib@: such a project has no
--   build directory, and its interfaces sit among its sources.
nameIndexFile
  :: AbsolutePath              -- ^ A source file of the project.
  -> TCM (Maybe AbsolutePath)
nameIndexFile = nameIndexFileIn . takeDirectory . filePath

-- | The same, from a directory inside the project rather than from one
--   of its source files.  This is what the builder has to go on: it
--   takes no input file.
nameIndexFileIn :: FilePath -> TCM (Maybe AbsolutePath)
nameIndexFileIn dir = fmap toIndex <$> nameIndexProjectRoot dir
  where
  toIndex root =
    mkAbsolute $ root </> "_build" </> version </> "agda" </> indexFileName

-- | The directory the builder has to be run in: the innermost enclosing
--   one with an @.agda-lib@ file.
nameIndexProjectRoot :: FilePath -> TCM (Maybe FilePath)
nameIndexProjectRoot = libToTCM . findProjectRoot

-- | The first line of the index file: what it was built from, and one
--   record per module it covers.
--
--   The manifest lives in the header rather than being spread over the
--   module lines so that the staleness scan can answer from a single
--   line, without touching the entries.
data IndexHeader = IndexHeader
  { ixFormat       :: !Int
  , ixAgdaVersion  :: Text
  , ixBuiltAt      :: !UTCTime
  , ixIncludePaths :: [FilePath]
      -- ^ The include path the index was built under, in the order of
      --   'projectIncludePaths': the project's own directories first,
      --   the libraries it depends on after them, Agda's builtin modules
      --   last.  A query run under a different one is answering about
      --   a different library; and the order is what breaks ties
      --   between hits of equal rank and popularity, so that the
      --   project's own @cong@ comes before a builtin's.
  , ixDefinitions  :: !Int
  , ixManifest     :: [IndexModuleInfo]
  }

-- | What the header records about one indexed module.
data IndexModuleInfo = IndexModuleInfo
  { imiModule      :: Text
      -- ^ The top-level module name, dotted.
  , imiSource      :: FilePath
  , imiFullHash    :: !Hash
      -- ^ @iFullHash@ of the interface the entries were rendered from.
      --   The full hash rather than the source hash, because a module's
      --   /rendered/ types change when an import does.
  , imiDefinitions :: !Int
  , imiFailed      :: Maybe Text
      -- ^ Why the builder could not index this module, for one it
      --   could not.  Such a module is /unindexable/ rather than stale:
      --   the index knows about it, and another build would not change
      --   that.  Absent for the ordinary case.
  }

-- | The entries of one module, on its own line of the index file.
data IndexModule = IndexModule
  { imModule  :: Text
  , imParams  :: Maybe Text
      -- ^ The enclosing module telescope, rendered once rather than
      --   into every type.
  , imEntries :: [IndexEntry]
  }

-- | One public definition.
data IndexEntry = IndexEntry
  { ieName         :: Text
      -- ^ The concrete name, as the defining module exports it.  This
      --   is what a query matches against.
  , ieQualifier    :: Maybe Text
      -- ^ How the name is reached inside its top-level module, when it
      --   is not reached directly: the record or submodule path one
      --   writes in front of 'ieName' after importing 'imModule'.
      --   Absent for the common case of a name defined in the top-level
      --   module itself.
  , ieQName        :: Text
      -- ^ The name as 'ieMentions' spells it: fully qualified, and
      --   through the anonymous modules a caller does not write.  It is
      --   what joins the two -- what makes 'ieUses' computable from the
      --   file, by a refresh that re-renders only part of it or by a
      --   client that never had the rest.
  , ieKind         :: Text
      -- ^ @KindOfName@: function, constructor, record field, ...
  , ieLine         :: !Word32
  , ieCol          :: !Word32
  , ieType         :: Text
      -- ^ Rendered against 'imParams', implicits hidden.
  , ieTypeImplicit :: Text
      -- ^ The same type with implicits shown: what you need before
      --   writing the application.
  , ieMentions     :: [Text]
      -- ^ The names occurring in the type, fully qualified.
  , ieUses         :: !Int
      -- ^ How many indexed definitions mention this one.
  }

-- | A whole index: the header and the entries of every module in it.
data NameIndex = NameIndex
  { ixHeader  :: IndexHeader
  , ixModules :: [IndexModule]
  }

------------------------------------------------------------------------
-- * JSON

-- | Hashes go out as hex rather than as numbers: a @Word64@ does not
--   survive a JSON parser that keeps numbers as doubles.
hashToText :: Hash -> Text
hashToText h = T.pack $ showHex h ""

hashFromText :: Text -> Maybe Hash
hashFromText t = case readHex (T.unpack t) of
  [(h, "")] -> Just h
  _         -> Nothing

instance ToJSON IndexHeader where
  toJSON h = object
    [ "format"       .= ixFormat h
    , "agdaVersion"  .= ixAgdaVersion h
    , "builtAt"      .= ixBuiltAt h
    , "includePaths" .= ixIncludePaths h
    , "definitions"  .= ixDefinitions h
    , "modules"      .= ixManifest h
    ]

instance FromJSON IndexHeader where
  parseJSON = withObject "index header" \ o -> IndexHeader
    <$> o .: "format"
    <*> o .: "agdaVersion"
    <*> o .: "builtAt"
    <*> o .: "includePaths"
    <*> o .: "definitions"
    <*> o .: "modules"

instance ToJSON IndexModuleInfo where
  toJSON i = object $
    [ "module"      .= imiModule i
    , "source"      .= imiSource i
    , "hash"        .= hashToText (imiFullHash i)
    , "definitions" .= imiDefinitions i
    ] ++
    [ "failed"      .= f | Just f <- [imiFailed i] ]

instance FromJSON IndexModuleInfo where
  parseJSON = withObject "indexed module" \ o -> IndexModuleInfo
    <$> o .: "module"
    <*> o .: "source"
    <*> (maybe (fail "not a hash") pure . hashFromText =<< o .: "hash")
    <*> o .:  "definitions"
    <*> o .:? "failed"

instance ToJSON IndexModule where
  toJSON m = object $
    [ "module"  .= imModule m
    , "entries" .= imEntries m
    ] ++
    [ "params"  .= p | Just p <- [imParams m] ]

instance FromJSON IndexModule where
  parseJSON = withObject "module entries" \ o -> IndexModule
    <$> o .:  "module"
    <*> o .:? "params"
    <*> o .:  "entries"

instance ToJSON IndexEntry where
  toJSON e = object $
    [ "name"         .= ieName e
    , "qname"        .= ieQName e
    , "kind"         .= ieKind e
    , "line"         .= ieLine e
    , "col"          .= ieCol e
    , "type"         .= ieType e
    , "typeImplicit" .= ieTypeImplicit e
    , "mentions"     .= ieMentions e
    , "uses"         .= ieUses e
    ] ++
    [ "qualifier"    .= q | Just q <- [ieQualifier e] ]

instance FromJSON IndexEntry where
  parseJSON = withObject "index entry" \ o -> IndexEntry
    <$> o .:  "name"
    <*> o .:? "qualifier"
    <*> o .:  "qname"
    <*> o .:  "kind"
    <*> o .:  "line"
    <*> o .:  "col"
    <*> o .:  "type"
    <*> o .:  "typeImplicit"
    <*> o .:? "mentions" .!= []
    <*> o .:? "uses"     .!= 0

------------------------------------------------------------------------
-- * Reading and writing

-- | Why an index could not be used.
data IndexError
  = IndexMissing
      -- ^ The project has no index file yet.
  | IndexUnreadable String
  | IndexWrongFormat !Int
      -- ^ Written by an Agda with a different index format.

indexErrorText :: IndexError -> String
indexErrorText = \case
  IndexMissing        -> "no index has been built for this project"
  IndexUnreadable err -> "the index file could not be read: " ++ err
  IndexWrongFormat n  -> concat
    [ "the index file is in format ", show n
    , ", not ", show indexFormatVersion ]

-- | Read the header, and the header only.
--
--   This is what a query pays on its way to the staleness scan: one
--   line, holding the manifest of every module the index covers.
readIndexHeader :: AbsolutePath -> IO (Either IndexError IndexHeader)
readIndexHeader f = readIndexLines f \case
  []         -> Left $ IndexUnreadable "the index file is empty"
  header : _ -> checkHeader =<< decodeLine "the index header" header

-- | Read the whole index.
readIndex :: AbsolutePath -> IO (Either IndexError NameIndex)
readIndex f = readIndexLines f \case
  [] -> Left $ IndexUnreadable "the index file is empty"
  header : ms -> do
    h  <- checkHeader =<< decodeLine "the index header" header
    ms' <- traverse (decodeLine "a module line of the index") ms
    Right $ NameIndex h ms'

-- Read strictly: a lazy read would defer its @IO@ errors past the
-- handler that is meant to turn a missing index into 'IndexMissing'.
readIndexLines
  :: AbsolutePath
  -> ([B.ByteString] -> Either IndexError a)
  -> IO (Either IndexError a)
readIndexLines f k =
  (k . B.lines <$> B.readFile (filePath f)) `catchIOError` \ e ->
    return $ Left $
      if isDoesNotExistError e then IndexMissing
      else IndexUnreadable (show e)

decodeLine :: FromJSON a => String -> B.ByteString -> Either IndexError a
decodeLine what = either (Left . IndexUnreadable . ((what ++ ": ") ++)) Right
                . eitherDecodeStrict

checkHeader :: IndexHeader -> Either IndexError IndexHeader
checkHeader h
  | ixFormat h == indexFormatVersion = Right h
  | otherwise                        = Left $ IndexWrongFormat (ixFormat h)

-- | Write an index out, header first and one line per module.
--
--   Written beside the target and renamed into place, so that a reader
--   never sees half an index.
writeIndex :: AbsolutePath -> NameIndex -> IO ()
writeIndex f ix = do
  createDirectoryIfMissing True (takeDirectory target)
  E.onException write $ ignoringIOErrors $ removeFile tmp
  where
  target = filePath f
  tmp    = target ++ ".tmp"
  write  = do
    BS.writeFile tmp $ BS.unlines $
      encode (ixHeader ix) : map encode (ixModules ix)
    renameFile tmp target
  ignoringIOErrors m = m `catchIOError` \ _ -> return ()

------------------------------------------------------------------------
-- * The coverage scan

-- | Why a module has no usable interface.
data NoInterface
  = NoInterfaceFile
      -- ^ It has never been checked here.
  | ForeignVersion !Word64
      -- ^ Its interface was written by a different Agda.
  | UnreadableInterface

noInterfaceName :: NoInterface -> Text
noInterfaceName = \case
  NoInterfaceFile     -> "noInterface"
  ForeignVersion{}    -> "foreignVersion"
  UnreadableInterface -> "unreadableInterface"

-- | What the scan found out about one source module.
data ModuleState
  = Current !Hash
      -- ^ The index covers it, and its interface still has that hash.
  | Moved !Hash
      -- ^ Its interface has moved since the index was built, or the
      --   index does not have the module at all.
  | Unindexable !Hash Text
      -- ^ The index tried this interface and could not read it.
      --   Building again would do the same, so it is not stale.
  | Unusable !NoInterface
      -- ^ There is nothing for the index to read.

data ScannedModule = ScannedModule
  { smModule   :: Text
  , smSource   :: AbsolutePath
  , smIFile    :: AbsolutePath
  , smState    :: ModuleState
  , smModified :: Bool
      -- ^ The source has been edited since the interface was written,
      --   so the interface -- and with it the index -- describes an
      --   older text.  Agda will re-check the module the next time it
      --   loads it; until then this is what keeps an answer from
      --   describing a library that no longer exists.
  }

-- | What a query can say about the index that answered it.
data Coverage = Coverage
  { covLocation :: Maybe AbsolutePath
      -- ^ Where the index lives, or 'Nothing' for a project with no
      --   build directory to hold one.
  , covIndex    :: Either IndexError IndexHeader
  , covScope    :: Maybe Text
      -- ^ The module prefix the scan was restricted to.
  , covModules  :: [ScannedModule]
  , covOrphans  :: [Text]
      -- ^ Modules the index covers whose source has since gone away.
  }

-- | The include path in the order the builder walks it and the index
--   records it: as Agda lists it -- the project's own directories, then
--   those of the libraries it depends on -- except that Agda's own
--   builtin modules come last, and deliberately so.  A project that
--   binds a builtin itself -- TypeTopology's @MLTT.Bool@ against
--   @Agda.Builtin.Bool@ -- can only have one of the two bindings, and
--   the project's own is the one its modules need.  Reaching the builtin
--   module first costs the whole library; reaching it last costs the
--   handful of builtin modules that clash, which are the ones the
--   project could not have imported anyway.
projectIncludePaths :: TCM [AbsolutePath]
projectIncludePaths = do
  incs <- optAbsoluteIncludePaths <$> commandLineOptions
  prim <- liftIO $ filePath <$> getPrimitiveLibDir
  let (builtins, ours) =
        List.partition ((prim `List.isPrefixOf`) . filePath) incs
  return $ ours ++ builtins

-- | Every source module on the include path, paired with the module
--   name its path spells, in the order the builder should reach them.
--
--   The enumeration keeps one file per /path/; this keeps one per
--   /module name/, which is what everything downstream is keyed on.  Two
--   include directories that spell the same module name are two files
--   Agda would never load both of, and left in they would put the module
--   into the index twice and count its @uses@ twice over.
enumerateProject :: TCM [(AbsolutePath, List1.List1 String)]
enumerateProject =
  nubOn snd <$> (liftIO . enumerateAgdaFiles =<< projectIncludePaths)

-- | Where on the include path a source file lives: the position of the
--   directory holding it, or the length of the path for a file under
--   none of them.  Include directories can nest, so the longest one
--   that holds the file is the one that counts.
--
--   This is what breaks ties between hits of equal rank and popularity,
--   and a client reading the index file computes it the same way from
--   the header's @includePaths@ and the manifest's @source@.
includeRank :: [FilePath] -> FilePath -> Int
includeRank paths src =
  case [ (List.length dir, i)
       | (i, p) <- zip [0 ..] paths
       , let dir = addTrailingPathSeparator p
       , dir `List.isPrefixOf` src ] of
    [] -> List.length paths
    xs -> snd $ List.maximum xs

-- | Does a module name lie under a module prefix?
--
--   @Foo@ covers @Foo@ and @Foo.Bar@, but not @Foobar@.
matchesModulePrefix :: Text -> Text -> Bool
matchesModulePrefix p m = p == m || (p <> ".") `T.isPrefixOf` m

-- | Walk the include path, work out which modules an index would have
--   to cover, and compare each against what the index recorded.
--
--   Costs a directory walk plus a stat and 24 bytes per module; nothing
--   is decompressed and nothing is type-checked.
scanCoverage
  :: AbsolutePath      -- ^ A source file of the project, to locate the index.
  -> Maybe Text        -- ^ Restrict the scan to this module prefix.
  -> TCM Coverage
scanCoverage = scanCoverageIn . takeDirectory . filePath

-- | The same, from a directory inside the project rather than from one
--   of its source files: what @--name-index-status@ has to go on.
scanCoverageIn :: FilePath -> Maybe Text -> TCM Coverage
scanCoverageIn dir scope = do
  location <- nameIndexFileIn dir
  header   <- liftIO $ maybe (return $ Left IndexMissing) readIndexHeader location
  let known = case header of
        Right h -> Map.fromList [ (imiModule i, i) | i <- ixManifest h ]
        Left{}  -> Map.empty
      inScope m = maybe True (`matchesModulePrefix` m) scope

  files <- enumerateProject
  let named = [ (m, f)
              | (f, ms) <- files
              , let m = T.intercalate "." $ map T.pack $ List1.toList ms
              , inScope m ]

  mods <- forM named \ (m, f) -> do
    ifile  <- toIFile =<< srcFromPath f
    prefix <- liftIO $ readInterfacePrefix ifile
    state  <- case prefix of
      -- A prefix that will not read is either an interface that is not
      -- there -- the ordinary case, a module nobody has checked -- or
      -- one that is there and broken, which is worth saying out loud.
      Nothing -> liftIO $ Unusable . unusable <$> doesFileExist (filePath ifile)
      Just (_sourceHash, full, ver)
        | ver /= currentInterfaceVersion -> return $ Unusable (ForeignVersion ver)
        | Just i <- Map.lookup m known
        , imiFullHash i == full          -> return $
            maybe (Current full) (Unindexable full) (imiFailed i)
        | otherwise                      -> return $ Moved full
    -- Whether the interface still describes the file beside it:
    -- Agda's own test, by the same hash, run here for the same reason
    -- it runs there -- an interface that fails it is about to be
    -- thrown away, and so is the part of the index built from it.
    --
    -- Hashing every source would double the scan in order to prove
    -- what is almost always so, that nothing has been edited.  The
    -- modification times say where to look, since Agda never writes an
    -- interface before reading the source it describes, and the hash
    -- decides.  A source restored with its old timestamp is the one
    -- case this misses, and it is one Agda's own loading catches.
    modified <- liftIO $ (`catchIOError` \ _ -> return False) $
      case prefix of
        Just (sourceHash, _, ver) | ver == currentInterfaceVersion -> do
          newer <- (>) <$> getModificationTime (filePath f)
                       <*> getModificationTime (filePath ifile)
          if not newer then return False else
            (/= sourceHash) <$> hashTextFile f
        _ -> return False
    return $ ScannedModule m f ifile state modified

  let present = Map.fromList [ (smModule s, ()) | s <- mods ]
      orphans = [ m | m <- Map.keys known, inScope m, not (Map.member m present) ]
  return Coverage
    { covLocation = location
    , covIndex    = header
    , covScope    = scope
    , covModules  = mods
    , covOrphans  = orphans
    }
  where
  unusable exists = if exists then UnreadableInterface else NoInterfaceFile

-- | How many entries the index holds for the modules in scope.
coverageDefinitions :: Coverage -> Int
coverageDefinitions cov = case covIndex cov of
  Left{}  -> 0
  Right h -> sum [ imiDefinitions i
                 | i <- ixManifest h
                 , maybe True (`matchesModulePrefix` imiModule i) (covScope cov) ]

-- | How much of the project the index does not account for: modules
--   whose interface has moved or that it never had, plus modules it has
--   whose source has gone away.  Zero is what "nothing to rebuild"
--   means.
coverageStale :: Coverage -> Int
coverageStale cov = List.length (covMoved cov) + List.length (covOrphans cov)

covCurrent, covMoved :: Coverage -> [ScannedModule]
covCurrent cov = [ s | s <- covModules cov, Current{} <- [smState s] ]
covMoved   cov = [ s | s <- covModules cov, Moved{}   <- [smState s] ]

covUnindexable :: Coverage -> [(ScannedModule, Text)]
covUnindexable cov =
  [ (s, why) | s <- covModules cov, Unindexable _ why <- [smState s] ]

covUnusable :: Coverage -> [(ScannedModule, NoInterface)]
covUnusable cov = [ (s, why) | s <- covModules cov, Unusable why <- [smState s] ]

-- | Modules whose source has moved past the interface the index was
--   built from.  Orthogonal to the states above: such a module can be
--   indexed, and current, and still describe text that is no longer
--   there.
covModified :: Coverage -> [ScannedModule]
covModified cov = List.filter smModified (covModules cov)

-- | How many module names a report spells out before it starts counting
--   instead.
namesShown :: Int
namesShown = 20

-- | The @index@ object every @findNames@ answer carries.
coverageValue :: Coverage -> Value
coverageValue cov = object $
  [ "location"      .= (filePath <$> covLocation cov)
  , "formatVersion" .= indexFormatVersion
  , "scope"         .= covScope cov
    -- The modules the index would have to cover.
  , "modules"       .= List.length (covModules cov)
  , "definitions"   .= coverageDefinitions cov
    -- Covered, and the interface has not moved since.
  , "indexed"       .= List.length (covCurrent cov)
    -- Checked, but the index does not match: it has moved, or the index
    -- never had it.
  , "stale"         .= List.length (covMoved cov)
    -- No usable interface: the module has not been checked here.
  , "unindexed"     .= List.length (covUnusable cov)
    -- The index tried and could not; building again would not help.
  , "unindexable"   .= List.length (covUnindexable cov)
    -- In the index, with no source file any more.
  , "removed"       .= List.length (covOrphans cov)
    -- Checked once, but edited since: the index describes the older
    -- text, and no build changes that until Agda checks the module
    -- again.
  , "modified"      .= List.length (covModified cov)
  , "staleModules"     .= map smModule (take namesShown (covMoved cov))
  , "unindexedModules" .=
      [ object [ "module" .= smModule s, "reason" .= noInterfaceName why ]
      | (s, why) <- take namesShown (covUnusable cov) ]
  , "unindexableModules" .=
      [ object [ "module" .= smModule s, "reason" .= why ]
      | (s, why) <- take namesShown (covUnindexable cov) ]
  , "modifiedModules" .= map smModule (take namesShown (covModified cov))
  ] ++ headerFields
  where
  headerFields = case covIndex cov of
    Right h ->
      [ "builtAt"      .= ixBuiltAt h
      , "agdaVersion"  .= ixAgdaVersion h
      , "includePaths" .= ixIncludePaths h
      ]
    Left err ->
      [ "builtAt" .= Null
      , "reason"  .= (case err of
          IndexMissing | Nothing <- covLocation cov ->
            "this project has no .agda-lib, so there is no build \
            \directory to hold an index"
          _ -> T.pack (indexErrorText err))
      ]

-- | The same thing, for the @text@ field.
coverageText :: Coverage -> [String]
coverageText cov = concat
  [ [ case covIndex cov of
        Right h -> "index:     built " ++ show (ixBuiltAt h)
        Left err -> "index:     " ++ case (err, covLocation cov) of
          (IndexMissing, Nothing) ->
            "unavailable (this project has no .agda-lib)"
          (IndexMissing, Just _) ->
            "not built yet; run `agda-client index` (the buildIndex method)"
          _ -> indexErrorText err ]
  , [ "modules:   " ++ show (List.length (covModules cov)) ++ scopeNote
      ++ ", " ++ show (List.length (covCurrent cov)) ++ " indexed"
      ++ ", " ++ show (List.length (covMoved cov)) ++ " stale"
      ++ ", " ++ show (List.length (covUnusable cov)) ++ " unindexed"
      ++ ", " ++ show (List.length (covUnindexable cov)) ++ " unindexable" ]
  , [ "removed:   " ++ show n | let n = List.length (covOrphans cov), n > 0 ]
  , [ "modified:  " ++ show n ++ " modules have been edited since they were "
      ++ "checked; they are indexed as they were"
    | let n = List.length (covModified cov), n > 0 ]
    -- What to do about it, once, rather than a number the reader has to
    -- know what to make of.  A refresh re-renders only what moved, so
    -- the advice is worth taking even for one module.
  , [ "refresh:   " ++ show n ++ (if n == 1 then " module" else " modules")
      ++ " to re-index; run `agda-client index` (the buildIndex method)"
    | Right{} <- [covIndex cov], let n = coverageStale cov, n > 0 ]
  ]
  where
  scopeNote = maybe "" (\ p -> " under " ++ T.unpack p) (covScope cov)

-- | Print the coverage report as one JSON object and exit
--   (@--name-index-status@).
--
--   JSON rather than prose because the mode exists for the clients that
--   are not the daemon: it is how the Emacs mode, or a client written
--   in another language, learns where the index file is without
--   re-deriving the build-directory layout for itself.  The object is
--   the same one a @findNames@ answer carries.
nameIndexStatus :: TCM ()
nameIndexStatus = do
  cwd <- liftIO getCurrentDirectory
  cov <- scanCoverageIn cwd Nothing
  liftIO $ BS.putStrLn $ encode $ coverageValue cov

------------------------------------------------------------------------
-- * The query
--
--   This is the specification a second client has to agree with, so it
--   is written here once and depended on from both ends.  Nothing in it
--   knows about Agda: a query matches a string, ranks it, and stops.

-- | What to look for.
data NameQuery = NameQuery
  { nqPattern :: Text
      -- ^ Matched against the name; substring by default.
  , nqModule  :: Maybe Text
      -- ^ Restrict to a top-level module prefix.  This also scopes the
      --   staleness scan, so a query about one corner of a library need
      --   not care that a distant part of it has moved.
  , nqGlob    :: Bool
      -- ^ Read @*@ and @?@ in the pattern as wildcards.  Off by
      --   default, because Agda names contain @*@.
  , nqType    :: [Text]
      -- ^ Names the type must mention, as @search@ takes them.
  , nqKinds   :: [Text]
      -- ^ Restrict to these kinds of name.
  , nqLimit   :: !Int
  }

-- | How well a hit matched.  This is the order hits are ranked in, and
--   the numbers are part of the wire format.
data MatchRank
  = ExactMatch
      -- ^ 1: the pattern is the whole name.
  | PrefixMatch
      -- ^ 2: the pattern is a proper prefix of the name.
  | ComponentMatch
      -- ^ 3: the pattern is a whole hyphen- or underscore-separated
      --   component of the name.
  | SubstringMatch
      -- ^ 4: the pattern occurs somewhere in the name.
  | MentionMatch
      -- ^ 5: the name does not match at all, but a name its type
      --   mentions does.  Only ever reached when the query asks about
      --   types, since on its own it would bury the name matches.
  deriving (Eq, Ord, Enum, Bounded, Show)

rankNumber :: MatchRank -> Int
rankNumber = succ . fromEnum

rankName :: MatchRank -> Text
rankName = \case
  ExactMatch     -> "exact"
  PrefixMatch    -> "prefix"
  ComponentMatch -> "component"
  SubstringMatch -> "substring"
  MentionMatch   -> "mentions"

-- | A compiled pattern.
data Matcher
  = Literal Text
      -- ^ Matched by equality; the ranks do the rest.
  | Glob [GlobPiece] (Maybe Text)
      -- ^ Matched anchored, plus the longest literal run in it, which
      --   every match must contain.

-- | A piece of a glob pattern.
data GlobPiece
  = GlobLit Text
  | GlobOne   -- ^ @?@
  | GlobMany  -- ^ @*@
  deriving (Eq, Show)

compilePattern
  :: Bool     -- ^ Read @*@ and @?@ as wildcards?
  -> Text
  -> Matcher
compilePattern glob p
  | not glob  = Literal p
  | otherwise = Glob pieces needle
  where
  pieces = parseGlob p
  needle = case [ l | GlobLit l <- pieces ] of
    [] -> Nothing
    ls -> Just $ List.maximumBy (comparing T.length) ls

-- | Split a glob pattern into literals and wildcards.  A backslash
--   escapes the character after it, which is how a literal @*@ is
--   written -- and Agda names do contain @*@, which is why globbing is
--   opt-in in the first place.
parseGlob :: Text -> [GlobPiece]
parseGlob = coalesce . go
  where
  go t = case T.uncons t of
    Nothing        -> []
    Just ('*', t') -> GlobMany : go t'
    Just ('?', t') -> GlobOne  : go t'
    Just ('\\', t') -> case T.uncons t' of
      Just (c, t'') -> GlobLit (T.singleton c) : go t''
      Nothing       -> [GlobLit "\\"]
    Just (c, t')   -> GlobLit (T.singleton c) : go t'
  coalesce (GlobLit a : GlobLit b : ps) = coalesce (GlobLit (a <> b) : ps)
  coalesce (q : ps)                     = q : coalesce ps
  coalesce []                           = []

-- | Does the glob match the whole of this text?
globMatch :: [GlobPiece] -> Text -> Bool
globMatch = go
  where
  go []             t = T.null t
  go (GlobLit l : ps) t = maybe False (go ps) $ T.stripPrefix l t
  go (GlobOne   : ps) t = maybe False (go ps . snd) $ T.uncons t
  -- The only place that backtracks, and the pattern is a query someone
  -- typed, so the pathological case is not worth guarding against.
  go (GlobMany  : ps) t = any (go ps) $ T.tails t

-- | The hyphen- or underscore-separated components of a name:
--   @is-basis-for@ is @is@, @basis@, @for@.
nameComponents :: Text -> [Text]
nameComponents = filter (not . T.null) . T.split (\ c -> c == '-' || c == '_')

-- | How well the pattern matches this name, or 'Nothing' if it does
--   not.  The first four ranks, in order.
rankOf :: Matcher -> Text -> Maybe MatchRank
rankOf (Literal p) n
  | n == p                          = Just ExactMatch
  | p `T.isPrefixOf` n              = Just PrefixMatch
  | p `List.elem` nameComponents n  = Just ComponentMatch
  | p `T.isInfixOf` n               = Just SubstringMatch
  | otherwise                       = Nothing
rankOf (Glob g _) n
  | globMatch g n                            = Just ExactMatch
  | any (globMatch g) (properPrefixes n)     = Just PrefixMatch
  | any (globMatch g) (nameComponents n)     = Just ComponentMatch
  | any (globMatch g) (substrings n)         = Just SubstringMatch
  | otherwise                                = Nothing
  where
  properPrefixes t = List.init (T.inits t)
  substrings t = [ s | u <- T.tails t, s <- List.drop 1 (T.inits u) ]

-- | Rank 5: the name itself does not match, but a name occurring in its
--   type does.  Matched against the unqualified part of the mentioned
--   name, which is what one would have typed.
mentionRank :: Matcher -> [Text] -> Maybe MatchRank
mentionRank m ms
  | any (isJust . rankOf m . T.takeWhileEnd (/= '.')) ms = Just MentionMatch
  | otherwise                                            = Nothing

-- | Does the type mention all of these?  This is @search@'s predicate,
--   without a scope to resolve the terms in, so a term matches a
--   mentioned name when it occurs in it -- qualifier included, so that
--   @UF.@ selects by module as well.
mentionsAll :: [Text] -> [Text] -> Bool
mentionsAll terms ms = all (\ t -> any (t `T.isInfixOf`) ms) terms

-- | Is this one of the kinds asked for?  An empty list asks for all of
--   them.
--
--   Agda spells its kinds @FunName@, @RecName@, @ConName@ and so on;
--   the comparison ignores case and a trailing @name@, so @FunName@,
--   @funname@, @Fun@ and @fun@ all name a function.
matchesKind :: [Text] -> Text -> Bool
matchesKind []  _ = True
matchesKind ks  k = any ((== norm k) . norm) ks
  where
  norm t = let l = T.toLower t in maybe l id (T.stripSuffix "name" l)

-- | One answer: an entry, with the module it came from and how it
--   matched.
data Hit = Hit
  { hitModule :: Text
  , hitSource :: FilePath
  , hitOrigin :: !Int
      -- ^ Where on the include path the module lives ('includeRank'):
      --   0 for the project's own directories.
  , hitParams :: Maybe Text
      -- ^ The module telescope the entry's type is rendered against.
  , hitRank   :: !MatchRank
  , hitEntry  :: IndexEntry
  }

data QueryResult = QueryResult
  { qrHits    :: [Hit]
      -- ^ Ranked, and cut to the query's limit.
  , qrMatched :: !Int
      -- ^ How many hits there were before the cut, so that a caller can
      --   tell a complete answer from a truncated one.
  }

-- | Answer a query from an index file.
--
--   The header is passed in rather than re-read: the coverage scan has
--   already paid for it, and it is what says where each module's source
--   is.
--
--   Most of the file is never decoded.  Every rank, the fifth included,
--   requires the pattern to occur literally somewhere in the module's
--   line, so a line that does not contain it cannot hold a hit -- and
--   the same goes for the module prefix, which occurs in the name of
--   every module it admits.  Both tests are a substring search over raw
--   bytes, against a JSON-escaped needle so that it is the encoding of
--   the pattern, not the pattern, that is looked for.
queryIndex
  :: AbsolutePath
  -> IndexHeader
  -> NameQuery
  -> IO (Either IndexError QueryResult)
queryIndex f header q = readIndexLines f \case
  []     -> Left $ IndexUnreadable "the index file is empty"
  _ : ls -> do
    hits <- concat <$> traverse ofLine (List.filter promising ls)
    let ranked = List.sortOn key hits
    return QueryResult
      { qrHits    = List.take (nqLimit q) ranked
      , qrMatched = List.length ranked
      }
  where
  matcher = compilePattern (nqGlob q) (nqPattern q)
  needles = List.map jsonNeedle $ concat
    [ maybe [] (: []) $ case matcher of
        Literal p  -> Just p
        Glob _ mps -> mps
    , maybe [] (: []) (nqModule q)
    ]
  promising l = all (`B.isInfixOf` l) needles

  paths   = ixIncludePaths header
  sources = Map.fromList
    [ (imiModule i, (imiSource i, includeRank paths (imiSource i)))
    | i <- ixManifest header ]

  ofLine l = do
    m <- decodeLine "a module line of the index" l
    let (source, origin) =
          Map.findWithDefault ("", List.length paths) (imModule m) sources
    return $ if not (inScope (imModule m)) then [] else
      [ Hit { hitModule = imModule m
            , hitSource = source
            , hitOrigin = origin
            , hitParams = imParams m
            , hitRank   = r
            , hitEntry  = e
            }
      | e <- imEntries m
      , matchesKind (nqKinds q) (ieKind e)
      , mentionsAll (nqType q) (ieMentions e)
      , Just r <- [ rankOf matcher (ieName e) `orElse`
                    (guard (not (List.null (nqType q))) >>
                     mentionRank matcher (ieMentions e)) ]
      ]

  inScope m = maybe True (`matchesModulePrefix` m) (nqModule q)

  -- Stable, and by something the caller can see: the rank, then the
  -- popularity that breaks ties inside it, then where the hit lives --
  -- the project's own modules before the libraries', and Agda's
  -- builtins last -- then the name.
  key h = ( rankNumber (hitRank h)
          , Down (ieUses (hitEntry h))
          , hitOrigin h
          , hitModule h
          , maybe "" id (ieQualifier (hitEntry h))
          , ieName (hitEntry h)
          , ieLine (hitEntry h)
          )

  orElse a b = maybe b Just a

-- | The bytes a text occupies inside a JSON string, quotes stripped.
--
--   Escaping is per character, so the encoding of a substring is a
--   substring of the encoding -- which is what makes searching the raw
--   line for it sound, whatever the encoder does with quotes,
--   backslashes and control characters.
jsonNeedle :: Text -> B.ByteString
jsonNeedle = BS.toStrict . BS.init . BS.tail . encode . String

-- | The @hits@ array of a @findNames@ answer.
hitsValue
  :: Bool          -- ^ Show implicit arguments in the types?
  -> [Hit]
  -> Value
hitsValue implicit hits = toJSON
  [ object $
      [ "name"     .= ieName e
      , "kind"     .= ieKind e
      , "module"   .= hitModule h
      , "file"     .= hitSource h
      , "line"     .= ieLine e
      , "column"   .= ieCol e
      , "type"     .= (if implicit then ieTypeImplicit e else ieType e)
      , "rank"     .= rankNumber (hitRank h)
      , "match"    .= rankName (hitRank h)
      , "uses"     .= ieUses e
      ] ++
      [ "qualifier" .= x | Just x <- [ieQualifier e] ] ++
      [ "params"    .= x | Just x <- [hitParams h] ]
  | h <- hits
  , let e = hitEntry h
  ]

-- | The same, for the @text@ field.
--
--   Types are cut off here and not in the answer proper: they run to a
--   hundred kilobytes in the worst case, and a list of hits is meant to
--   be read.
hitsText :: NameQuery -> Bool -> QueryResult -> [String]
hitsText q implicit r = concat
  [ [ headline ]
  , concatMap hit (qrHits r)
  ]
  where
  n = List.length (qrHits r)
  -- Quoted, but not 'show'n: the pattern is an Agda name and Agda names
  -- are not ASCII.
  pat = "\"" ++ T.unpack (nqPattern q) ++ "\""
  headline
    | n == 0 = "findNames: no hits for " ++ pat
    | n == qrMatched r = concat
        [ "findNames: ", plural n "hit", " for ", pat ]
    | otherwise = concat
        [ "findNames: ", plural n "hit", " for ", pat
        , " (of ", show (qrMatched r), " matched)" ]

  hit h = concat
    [ [ concat
        [ "  ", T.unpack (full h), " : ", clip (theType e) ] ]
    , [ concat
        [ "    ", T.unpack (ieKind e), ", ", T.unpack (rankName (hitRank h))
        , ", ", plural (ieUses e) "use", " -- ", hitSource h
        , ":", show (ieLine e), ",", show (ieCol e) ] ]
    , [ "    params " ++ clip (T.unpack p) | Just p <- [hitParams h] ]
    ]
    where e = hitEntry h

  full h = T.intercalate "." $ concat
    [ [ hitModule h ]
    , maybe [] (: []) (ieQualifier (hitEntry h))
    , [ ieName (hitEntry h) ]
    ]

  theType e = T.unpack $ if implicit then ieTypeImplicit e else ieType e

  clip s | List.length s <= typeWidth = s
         | otherwise = List.take typeWidth s ++ " ..."

  plural k what = show k ++ " " ++ what ++ (if k == 1 then "" else "s")

-- | How much of a type a hit list shows before eliding it.
typeWidth :: Int
typeWidth = 160

------------------------------------------------------------------------
-- * The builder

-- | What the builder learned about one module, before @uses@ is known.
data BuiltModule = BuiltModule
  { bmInfo    :: IndexModuleInfo
  , bmParams  :: Maybe Text
  , bmEntries :: [IndexEntry]
  }

-- | What became of one module the index has to cover.
data IndexedModule
  = Rendered BuiltModule
      -- ^ Read off its interface and rendered, now.
  | Reused IndexModuleInfo (Maybe IndexModule)
      -- ^ Kept from the index on disk, its interface not having moved
      --   since.  The entries are 'Nothing' for a module the previous
      --   build could not read: it is still unindexable, and still for
      --   the same reason.
  | Unreadable IndexModuleInfo Text
      -- ^ It would not load now.  Recorded against the hash it choked
      --   on, so that the next scan calls it unindexable rather than
      --   stale -- something no rebuild would fix.
  | Skipped Text
      -- ^ It would not load and has no readable interface either, so
      --   there is nothing to record it against.  The scan calls it
      --   unindexed, which is the honest answer: check it once.

-- | Build the index of everything the project in the current directory
--   could import (@--build-name-index@).
--
--   This is a load rather than a re-check: 'Imp.getNonMainModuleInfo'
--   reads the @.agdai@ when it is current, and only type-checks a
--   module whose interface has gone stale.  What the builder pays
--   beyond that load is reification -- every public type is rendered
--   twice, with implicits hidden and shown -- and it pays it once, so
--   that a query afterwards is a string match against a file.
--
--   'RefreshNameIndex' re-renders only the modules whose interfaces
--   have moved since the index was built, and keeps the rest of it
--   verbatim.  That is the whole of "restrict the builder to a stale
--   set and its dependents": @iFullHash@ combines a module's source
--   hash with those of its imports, so a module whose dependency
--   changed has moved too, and the dependents come along without being
--   tracked.  What a refresh cannot keep is @uses@, which counts across
--   the whole index; it is recomputed every time, from the @mentions@
--   of kept and re-rendered entries alike.
--
--   A module that will not load is reported and skipped.  Half an index
--   with an account of what is missing is worth more here than no index
--   at all, and the coverage scan says the same thing again at query
--   time.
buildNameIndex :: NameIndexBuild -> TCM ()
buildNameIndex mode = do
  cwd  <- liftIO getCurrentDirectory
  out  <- nameIndexFileIn cwd >>= \case
    Just f  -> return f
    Nothing -> throwError $ GenericException $ concat
      [ "Cannot build a name index for ", cwd, ": no .agda-lib file was "
      , "found there or above it, so there is no build directory to hold "
      , "the index." ]

  -- The library's own pragmas, as @--build-library@ applies them: an
  -- interface checked under them is only reusable under them.
  libToTCM (getAgdaLibFile cwd) >>= \case
    [AgdaLibFile{ _libPragmas = libOpts }] -> checkAndSetOptionsFromPragma libOpts
    _ -> return ()

  Imp.importPrimitiveModules

  incs  <- projectIncludePaths
  files <- enumerateProject
  let total = List.length files

  -- What of the index on disk a refresh may keep.  An index built by
  -- another Agda, or under another include path, is answering about
  -- another library and is not reused.
  previous <- case mode of
    RebuildNameIndex -> return Nothing
    RefreshNameIndex -> liftIO (readIndex out) <&> \case
      Right ix
        | ixAgdaVersion (ixHeader ix) == T.pack version
        , ixIncludePaths (ixHeader ix) == map filePath incs
        -> Just ix
      _ -> Nothing
  let knownInfo = Map.fromList
        [ (imiModule i, i) | Just ix <- [previous], i <- ixManifest (ixHeader ix) ]
      knownMods = Map.fromList
        [ (imModule m, m) | Just ix <- [previous], m <- ixModules ix ]

  -- First the 24-byte prefix of every interface, which is what says
  -- whether the index still matches it.  Nothing is decompressed here,
  -- so this is the scan a query pays for, not a build.
  scanned <- forM files \ (path, parts) -> do
    m <- topLevelModuleName RawTopLevelModuleName
      { rawModuleNameRange    = noRange
      , rawModuleNameParts    = fmap T.pack parts
      , rawModuleNameInferred = False
      }
    let name = T.pack $ prettyShow m
    ifile  <- toIFile =<< srcFromPath path
    prefix <- liftIO $ readInterfacePrefix ifile
    let full = case prefix of
          Just (_sourceHash, h, ver) | ver == currentInterfaceVersion -> Just h
          _                                                          -> Nothing
        kept = do
          h    <- full
          info <- Map.lookup name knownInfo
          guard $ imiFullHash info == h
          return info
    return (path, m, name, full, kept)

  let todo  = [ x | x@(_, _, _, _, Nothing) <- scanned ]
      reuse = List.length scanned - List.length todo
  alwaysReportSLn "nameindex" 1 $ concat
    [ "Indexing ", show (List.length todo), " of ", show total
    , " modules on the include path."
    , if reuse == 0 then "" else
        concat [ " (", show reuse, " already indexed.)" ] ]

  -- The order is the enumeration's, which puts Agda's own builtin
  -- modules last; a refresh keeps it, so a module reached from the
  -- index sits where a rebuilt one would.
  rebuilt <- fmap Map.fromList $ forM (zip [1 :: Int ..] todo)
    \ (k, (path, m, name, full, _)) -> do
      -- Enough to see that it is making progress, not so much that the
      -- interesting lines -- the modules that would not load -- scroll
      -- away.
      when (k == 1 || k == List.length todo || k `mod` 50 == 0) $
        alwaysReportSLn "nameindex" 1 $ concat
          [ "[", show k, "/", show (List.length todo), "] ", prettyShow m ]
      -- A module the builder could not read is recorded too, against
      -- the hash of the interface it choked on.  Left out, it would
      -- come back as /stale/ at every query -- a rebuild that would
      -- fix it, which no rebuild will -- and the up-to-date answer
      -- would never be reachable for a project that shadows a
      -- builtin.
      let unreadable msg = case full of
            Nothing -> Skipped msg
            Just h  -> Unreadable IndexModuleInfo
              { imiModule      = name
              , imiSource      = filePath path
              , imiFullHash    = h
              , imiDefinitions = 0
              , imiFailed      = Just msg
              } msg
      -- An @__IMPOSSIBLE__@ is a plain Haskell exception rather than a
      -- 'TCErr', so 'catchError' does not see it: uncaught, one module
      -- Agda cannot make sense of takes the builder process down and
      -- the project is left with no index at all.  Caught here it
      -- costs that one module, like every other failure to load.
      -- 'catchImpossible' at 'TCM' also puts the state back to what it
      -- was before the module.
      r <- (   (Rendered <$> indexModule path m)
                 `catchError` \ err -> unreadable . render1 <$> prettyError err
           ) `catchImpossible` \ imp ->
                 return $ unreadable $ T.unwords $ T.words $ T.pack $ show imp
      return (name, r)

  let indexed =
        [ (m, r)
        | (_, m, name, _, kept) <- scanned
        , r <- case kept of
            Just info -> [ Reused info (Map.lookup name knownMods) ]
            Nothing   -> maybe [] (: []) (Map.lookup name rebuilt)
        ]
      failed = [ (m, msg)
               | (m, r) <- indexed
               , msg <- case r of
                   Unreadable _ msg -> [ msg ]
                   Skipped msg      -> [ msg ]
                   _                -> []
               ]

  -- One pass over the finished entries turns `mentions` into `uses`.
  -- Kept entries count for it and are counted, which is what the
  -- qualified name in each entry is there for: a refresh gets the same
  -- numbers a rebuild would.
  let entriesOf = \case
        Rendered b        -> bmEntries b
        Reused _ (Just m) -> imEntries m
        _                 -> []
      counts = Map.fromListWith (+)
        [ (q, 1 :: Int) | (_, r) <- indexed, e <- entriesOf r, q <- ieMentions e ]
      recount e = e{ ieUses = Map.findWithDefault 0 (ieQName e) counts }
      lines' =
        [ line
        | (_, r) <- indexed
        , line <- case r of
            Rendered b -> [ IndexModule
              { imModule  = imiModule (bmInfo b)
              , imParams  = bmParams b
              , imEntries = map recount (bmEntries b)
              } ]
            Reused _ (Just m) -> [ m{ imEntries = map recount (imEntries m) } ]
            _                 -> []
        ]
      infos =
        [ i
        | (_, r) <- indexed
        , i <- case r of
            Rendered b     -> [ bmInfo b ]
            Reused i _     -> [ i ]
            Unreadable i _ -> [ i ]
            Skipped{}      -> []
        ]

  now <- liftIO getCurrentTime
  let defs  = sum $ map imiDefinitions infos
      index = NameIndex
        { ixHeader = IndexHeader
            { ixFormat       = indexFormatVersion
            , ixAgdaVersion  = T.pack version
            , ixBuiltAt      = now
            , ixIncludePaths = map filePath incs
            , ixDefinitions  = defs
            , ixManifest     = infos
            }
        , ixModules = lines'
        }
  liftIO $ writeIndex out index

  unless (List.null failed) do
    alwaysReportSLn "nameindex" 1 $ concat
      [ show (List.length failed), " modules could not be loaded and are "
      , "not in the index:" ]
    forM_ (take namesShown failed) \ (m, msg) ->
      alwaysReportSLn "nameindex" 1 $
        concat [ "  ", prettyShow m, ": ", T.unpack msg ]
    when (List.length failed > namesShown) $
      alwaysReportSLn "nameindex" 1 $ concat
        [ "  (and ", show (List.length failed - namesShown), " more)" ]

  alwaysReportSLn "nameindex" 1 $ concat
    [ "Wrote ", filePath out, ": ", show (List.length infos), " modules, "
    , show defs, " definitions."
    , if List.null failed then "" else
        concat [ " (", show (List.length failed), " could not be loaded.)" ] ]

-- | Index one module: load it, then read its public surface off the
--   interface and render it.
--
--   A module that checks with error warnings -- unsolved metas or
--   unsolved constraints, say -- writes no interface, and Agda will
--   not reuse such a check: the next module that imports it checks it
--   a second time.  In the builder's one persistent state that second
--   check is fatal, because the first one left the module's
--   definitions among the imports and putting them into the signature
--   as well is @Ambiguous name@ -- an internal error, and the whole
--   build lost for one module.  So a module like that is checked,
--   rendered, and then rolled back out of the state again, which
--   leaves the run as it would have been had the module not been
--   indexed yet: whoever imports it checks it once, fails on its
--   warnings as under plain @agda@, and is recorded as unreadable like
--   any other module that will not load.  The decoded interfaces are
--   kept across the rollback -- they are a cache, and the modules they
--   belong to are merged back out of it when they are next imported --
--   so the price is re-checking the offending module, not everything
--   it imports.
indexModule :: AbsolutePath -> TopLevelModuleName -> TCM BuiltModule
indexModule path m = do
  saved <- getTC
  mi    <- Imp.getNonMainModuleInfo m Nothing
  built <- renderModule path m $ miInterface mi
  unless (null $ miWarnings mi) do
    decoded <- getDecodedModules
    putTC saved
    setDecodedModules decoded
  return built

-- | Read a loaded module's public surface off its interface and render
--   it.
renderModule :: AbsolutePath -> TopLevelModuleName -> Interface -> TCM BuiltModule
renderModule path m i = do
  -- The enclosing telescope, rendered once for the whole module rather
  -- than into each of its types.  Every definition's type begins with
  -- it, so the types below drop exactly this many binders; a definition
  -- in a parameterised submodule keeps the binders that submodule adds,
  -- which is what a caller has to supply anyway.
  let top = iModuleName i
  tel <- lookupSection top

  -- Render inside the module's own scope, which is how its author
  -- reads these types and how the library's own documentation shows
  -- them.  Against no scope at all every name comes out maximally
  -- qualified, which on TypeTopology means @Agda.Primitive.Level@ for
  -- @Level@ and a type ten times as long -- unreadable, and an index of
  -- 37 megabytes rather than 4.  Nothing is lost by it: the hit itself
  -- is reported by module and name, and 'ieMentions' carries the fully
  -- qualified name of everything the type refers to.
  evalWithScope (iInsideScope i) do
   recomputeInverseScope

   params <- if null tel then return Nothing else
     Just . render1 <$> prettyTCM tel

   entries <- forM (candidates i) \ (qual, x, a) -> do
    let q = anameName a
        (line, col) = startOf $ nameBindingSite $ qnameName q
    (ty, tyImp, ms) <- renderEntry (size tel) q
    return IndexEntry
      { ieName         = T.pack $ prettyShow x
      , ieQualifier    = if T.null qual then Nothing else Just qual
      , ieQName        = T.pack $ prettyShow q
      , ieKind         = T.pack $ show $ anameKind a
      , ieLine         = line
      , ieCol          = col
      , ieType         = ty
      , ieTypeImplicit = tyImp
      , ieMentions     = ms
      , ieUses         = 0
      }

   return BuiltModule
    { bmInfo = IndexModuleInfo
        { imiModule      = T.pack $ prettyShow m
        , imiSource      = filePath path
        , imiFullHash    = iFullHash i
        , imiDefinitions = List.length entries
        , imiFailed      = Nothing
        }
    , bmParams  = params
    , bmEntries = entries
    }

-- | The public names an interface defines, each under the shortest
--   qualification that reaches it.
--
--   Keyed on the defining 'QName', not on what the scopes export: a
--   module's scope also carries what it re-exports from what it opens,
--   and counting those inflates a dense module several times over.  The
--   'NameId' of a name records the top-level module it was created in,
--   so the test is a comparison of two hashes.
candidates :: Interface -> [(Text, C.Name, AbstractName)]
candidates i =
  List.sortOn (\ (qual, x, _) -> (qual, prettyShow x)) $
  Map.elems $ Map.fromListWith shorter
    [ (anameName a, (qual, x, a))
    | (mname, scope) <- Map.toList $ iScope i
    , Just qual <- [qualifierIn (iModuleName i) mname]
    , (x, as) <- Map.toList $ allNamesInScope scope
    , a <- List1.toList as
    , definedHere $ anameName a
    ]
  where
  hash = moduleNameId $ iTopLevelModuleName i
  definedHere q = case nameId $ qnameName q of NameId _ h -> h == hash
  shorter new@(q1, x1, _) old@(q2, x2, _)
    | (T.length q1, q1, prettyShow x1) < (T.length q2, q2, prettyShow x2) = new
    | otherwise = old

-- | How a name in the given submodule is written after importing the
--   top-level module: the submodule path below it, minus the anonymous
--   modules, whose contents the enclosing module has already opened.
--
--   'Nothing' if the submodule is not under the top-level module at all,
--   which the scopes of an interface should not contain.
qualifierIn :: ModuleName -> ModuleName -> Maybe Text
qualifierIn top sub = do
  let tops = mnameToList top
      subs = mnameToList sub
  guard $ map nameId tops `List.isPrefixOf` map nameId subs
  return $ T.intercalate "." $ map (T.pack . prettyShow)
    [ x | n <- drop (List.length tops) subs
        , let x = nameConcrete n
        , not $ C.isNoName x ]

-- | The two renderings of a definition's type, and the names it
--   mentions.
--
--   Empty for a name with no type in the signature -- a pattern
--   synonym, which lives in @iPatternSyns@ rather than among the
--   definitions.
renderEntry :: Int -> QName -> TCM (Text, Text, [Text])
renderEntry nparams q = getConstInfo' q >>= \case
  Left{}  -> return ("", "", [])
  Right d -> do
    let t = defType d
    TelV tel core <- telViewUpTo nparams t
    (hidden, shown) <- addContext tel $ (,)
      <$> (render1 <$> withShowAllArguments' False (prettyTCM core))
      <*> (render1 <$> withShowAllArguments' True  (prettyTCM core))
    -- Search About's own predicate, as a set of names rather than as
    -- text to be parsed back: a generic traversal of the type, minus
    -- the definition itself, which would only inflate its own `uses`.
    let ms = [ T.pack $ prettyShow n
             | n <- Set.toList (namesIn t :: Set.Set QName), n /= q ]
    return (hidden, shown, ms)

-- | Render on one line: the index is a line-oriented file, and a hit is
--   a line of a result list.
render1 :: Doc -> Text
render1 = T.pack . renderStyle style{ mode = OneLineMode }

-- | Where a range starts, as the 1-based line and column an editor
--   wants.  @(0, 0)@ for a name with no binding site, which serialised
--   ranges do not have but generated names might.
startOf :: Range -> (Word32, Word32)
startOf r = case rangeIntervals r of
  iv : _ -> (posLine (iStart iv), posCol (iStart iv))
  []     -> (0, 0)
