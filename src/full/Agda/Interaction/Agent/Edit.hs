{-# OPTIONS_GHC -Wunused-imports #-}

-- | Turning the result of a proof-mutating command into a checked
--   source edit.
--
--   The Emacs frontend tells the editor what to do with its buffer; the
--   agent frontend owns the file on disk, so applying a give, refine or
--   solve result means rewriting the source itself.  A mutation is
--   therefore run as a small transaction:
--
--   1. take a 'Snapshot' of the file /before/ running the command;
--   2. plan one 'TextEdit' per goal from the hole's range and the text
--      Agda computed, checking that the range really does hold a hole
--      (this is the expected-old-text check);
--   3. check that the edits do not overlap ('checkOverlaps');
--   4. check that the file has not changed while the command was
--      running ('snapshotUnchanged') — if it has, nothing is written;
--   5. write the new source atomically ('writeSource').
--
--   The caller then reloads, so that the goal identifiers it returns
--   refer to the file as it now is.
--
--   Positions are the ones Agda reports: code-point offsets into the
--   file /after/ line endings have been normalised and a byte order
--   mark stripped (see "Agda.Utils.IO.UTF8").  A snapshot keeps the raw
--   text alongside the normalised one and translates between the two,
--   so that editing one hole in a CRLF file does not rewrite every line
--   of it.

module Agda.Interaction.Agent.Edit
  ( -- * Snapshots
    Snapshot(..)
  , takeSnapshot
  , snapshotUnchanged
  , offsetOfLineCol
  , lineColOfOffset
    -- * Edits
  , TextEdit(..)
  , planHoleEdit
  , planClauseEdit
  , checkOverlaps
  , applyEdits
  , writeSource
  , editValue
  ) where

import Control.Exception          ( catch, throwIO )
import Control.Monad              ( unless, when )

import Data.Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Int                   ( Int64 )
import Data.List qualified as List
import Data.Text.Lazy             ( Text )
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Word                  ( Word32 )

import System.Directory           ( copyPermissions, renameFile )
import System.FilePath            ( takeDirectory, takeFileName, (</>) )
import System.IO.Error            ( isDoesNotExistError )

import Agda.Interaction.Agent.Protocol
  ( agentRange, apCol, apOffset, arEnd, arStart )

import Agda.Syntax.Position       ( Range )

import Agda.Utils.FileName        ( AbsolutePath, filePath )
import Agda.Utils.Hash            ( Hash, hashText, hashTextFile )
import Agda.Utils.IO.UTF8         ( convertLineEndings, stripUtf8Bom )

------------------------------------------------------------------------
-- * Snapshots

-- | The source of a file as it was when a command started running.
data Snapshot = Snapshot
  { snapPath   :: AbsolutePath
  , snapBom    :: !Bool
      -- ^ The file starts with a UTF-8 byte order mark.
  , snapRaw    :: Text
      -- ^ Decoded, byte order mark removed, line endings as in the file.
  , snapText   :: Text
      -- ^ Line endings normalised: the text Agda's positions index.
  , snapSimple :: !Bool
      -- ^ 'snapRaw' and 'snapText' have the same offsets.
  , snapEol    :: Text
      -- ^ The line ending that inserted text should use.
  , snapHash   :: !Hash
      -- ^ Hash of 'snapText', comparable with 'hashTextFile'.
  }

-- | Read a file, keeping both the raw and the normalised text.
takeSnapshot :: AbsolutePath -> IO Snapshot
takeSnapshot path = do
  bytes <- BS.readFile $ filePath path
  let stripped = stripUtf8Bom bytes
  raw <- case T.decodeUtf8' stripped of
    Right t -> return t
    Left _  -> throwIO $ userError $
      "failed to read " ++ filePath path ++ " as UTF-8"
  let norm = convertLineEndings raw
  return Snapshot
    { snapPath   = path
    , snapBom    = BS.length stripped /= BS.length bytes
    , snapRaw    = raw
    , snapText   = norm
    , snapSimple = not $ T.any (== '\r') raw
    , snapEol    = if T.isInfixOf "\r\n" raw then "\r\n" else "\n"
    , snapHash   = hashText norm
    }

-- | Is the file still what it was when the snapshot was taken?
snapshotUnchanged :: Snapshot -> IO Bool
snapshotUnchanged snap = (snapHash snap ==) <$> hashTextFile (snapPath snap)

-- | The offset in 'snapRaw' corresponding to an offset in 'snapText'.
--
--   The only conversion that changes offsets is CRLF to LF, so files
--   without a carriage return — that is, nearly all of them — need no
--   translation at all.
rawOffset :: Snapshot -> Int64 -> Int64
rawOffset snap n
  | snapSimple snap = n
  | otherwise       = go 0 0 $ T.unpack $ snapRaw snap
  where
  go :: Int64 -> Int64 -> String -> Int64
  go !ri !ni cs
    | ni >= n = ri
    | otherwise = case cs of
        '\r' : '\n' : rest -> go (ri + 2) (ni + 1) rest
        _          : rest  -> go (ri + 1) (ni + 1) rest
        []                 -> ri

-- | The code-point offset of a 1-based line and column in a normalised
--   text, or 'Nothing' if the text has no such position.
--
--   A column just past the end of a line is accepted: that is where a
--   position at the end of a token sits.
offsetOfLineCol :: Text -> Word32 -> Word32 -> Maybe Int64
offsetOfLineCol t line col
  | line < 1 || col < 1                = Nothing
  | fromIntegral line > length ls      = Nothing
  | fromIntegral col > T.length l + 1  = Nothing
  | otherwise = Just $ start + fromIntegral col - 1
  where
  ls    = T.splitOn "\n" t
  l     = ls !! (fromIntegral line - 1)
  -- The line breaks are one character each, hence the @+ 1@.
  start = sum [ T.length x + 1 | x <- take (fromIntegral line - 1) ls ]

-- | The 1-based line and column of a code-point offset.
lineColOfOffset :: Text -> Int64 -> (Word32, Word32)
lineColOfOffset t off = (fromIntegral (length ls), fromIntegral (T.length (last ls) + 1))
  where
  ls = T.splitOn "\n" $ T.take (max 0 off) t

------------------------------------------------------------------------
-- * Edits

-- | The replacement of one stretch of source text by another.
data TextEdit = TextEdit
  { edRange :: Range
      -- ^ The range that is replaced, as Agda reports it.
  , edStart :: !Int64
      -- ^ Its start, as a code-point offset into 'snapText'.
  , edEnd   :: !Int64
      -- ^ Its end, exclusive.
  , edOld   :: Text
      -- ^ What the snapshot has there.
  , edNew   :: Text
      -- ^ What replaces it.
  }

-- | Plan the replacement of a goal by new text.
--
--   Fails unless the goal's range really holds a hole, which is what
--   makes a stale range a clean error rather than a mangled file.
--
--   Text that spills onto further lines is indented to the column of
--   the hole, as the Emacs mode indents what it inserts; without that,
--   a two-line expression would end the declaration it belongs to.
planHoleEdit :: Snapshot -> Range -> Text -> Either String TextEdit
planHoleEdit snap r new = do
  ar <- maybe (Left "the goal has no source range") Right $ agentRange r
  let s, e :: Int64
      s     = fromIntegral $ apOffset $ arStart ar
      e     = fromIntegral $ apOffset $ arEnd ar
      total = T.length (snapText snap)
  unless (0 <= s && s <= e && e <= total) $
    Left "the goal's range is not inside the file"
  let old = T.take (e - s) $ T.drop s $ snapText snap
  unless (isHole old) $ Left $
    "the goal's range holds " ++ show (T.unpack old) ++ ", not a hole"
  return TextEdit
    { edRange = r
    , edStart = s
    , edEnd   = e
    , edOld   = old
    , edNew   = indentTo (apCol (arStart ar)) new
    }
  where
  -- A goal is written either @?@ or @{! ... !}@.
  isHole t = t == "?" ||
    (T.length t >= 4 && T.isPrefixOf "{!" t && T.isSuffixOf "!}" t)

-- | Indent every line but the first to the given (1-based) column.
indentTo :: Word32 -> Text -> Text
indentTo col t = case T.splitOn "\n" t of
  first : rest@(_ : _) -> T.intercalate "\n" $ first : map pad rest
  _                    -> t
  where
  indent = T.replicate (fromIntegral col - 1) " "
  pad l  = if T.null l then l else indent <> l

-- | Plan the replacement of a clause by the clauses a case split
--   produced.
--
--   What is replaced here is a whole clause rather than a hole, so the
--   check that the source still says what Agda thinks it says is that
--   the clause's range covers the goal that was split.
--
--   Function clauses, and the clauses of a @λ where@, go on lines of
--   their own, indented to the clause that was split; the clauses of an
--   inline @λ { … }@ are separated by @;@ instead.  This is the rule
--   the Emacs mode uses, and it reads the same way off the source: a
--   clause that starts its line stays on its own line.
planClauseEdit
  :: Snapshot
  -> Range    -- ^ The clause to replace.
  -> Range    -- ^ The goal that was split; must lie inside it.
  -> Bool     -- ^ Is the clause one of an extended lambda?
  -> [Text]   -- ^ The clauses that replace it, one line each.
  -> Either String TextEdit
planClauseEdit snap clause hole extendedLambda cs = do
  ar <- maybe (Left "the clause has no source range") Right $ agentRange clause
  hr <- maybe (Left "the goal has no source range")   Right $ agentRange hole
  let s, e, hs, he :: Int64
      s     = fromIntegral $ apOffset $ arStart ar
      e     = fromIntegral $ apOffset $ arEnd   ar
      hs    = fromIntegral $ apOffset $ arStart hr
      he    = fromIntegral $ apOffset $ arEnd   hr
      total = T.length (snapText snap)
  when (List.null cs) $ Left "the split produced no clauses"
  unless (0 <= s && s <= e && e <= total) $
    Left "the clause's range is not inside the file"
  unless (s <= hs && he <= e) $
    Left "the clause's range does not cover the goal that was split"
  let inline = extendedLambda && not (startsItsLine (snapText snap) s)
  return TextEdit
    { edRange = clause
    , edStart = s
    , edEnd   = e
    , edOld   = T.take (e - s) $ T.drop s $ snapText snap
    , edNew   = if inline
                then T.intercalate " ; " cs
                else indentTo (apCol (arStart ar)) (T.intercalate "\n" cs)
    }

-- | Is there nothing but whitespace before this offset on its line?
startsItsLine :: Text -> Int64 -> Bool
startsItsLine t off =
  T.all (\ c -> c == ' ' || c == '\t') $ T.takeWhileEnd (/= '\n') $ T.take off t

-- | Reject edits that would step on each other.
checkOverlaps :: [TextEdit] -> Either String ()
checkOverlaps eds =
  case [ (a, b) | (a, b) <- zip sorted (drop 1 sorted), edEnd a > edStart b ] of
    []          -> Right ()
    (a, _) : _  -> Left $
      "two edits overlap at offset " ++ show (edStart a)
  where
  sorted = List.sortOn edStart eds

-- | Apply the edits to the raw text of the snapshot.
--
--   Applying them from the back means the offsets of the edits still to
--   come are not disturbed.
applyEdits :: Snapshot -> [TextEdit] -> Text
applyEdits snap = List.foldl' apply (snapRaw snap) . List.sortOn (negate . edStart)
  where
  apply raw ed = T.concat
    [ T.take (rawOffset snap $ edStart ed) raw
    , eol (edNew ed)
    , T.drop (rawOffset snap $ edEnd ed) raw
    ]
  eol | snapEol snap == "\n" = id
      | otherwise            = T.replace "\n" (snapEol snap)

-- | Replace the file's contents, atomically: the new text is written to
--   a sibling file which is then renamed over the original, so a
--   concurrent reader sees either the old or the new source, never half
--   of each.
writeSource :: Snapshot -> Text -> IO ()
writeSource snap new = do
  BS.writeFile tmp $ (if snapBom snap then bom else BS.empty) <> T.encodeUtf8 new
  copyPermissions path tmp `catch` \ e ->
    if isDoesNotExistError e then return () else throwIO e
  renameFile tmp path
  where
  path = filePath $ snapPath snap
  tmp  = takeDirectory path </> ("." ++ takeFileName path ++ ".agda-edit.tmp")
  bom  = BS.pack [ 0xEF, 0xBB, 0xBF ]

-- | How an applied edit is reported.
editValue :: TextEdit -> Value
editValue ed = object
  [ "range"   .= agentRange (edRange ed)
  , "oldText" .= T.toStrict (edOld ed)
  , "newText" .= T.toStrict (edNew ed)
  ]
