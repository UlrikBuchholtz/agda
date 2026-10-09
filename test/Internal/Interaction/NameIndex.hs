{-# LANGUAGE TemplateHaskell #-}

-- | The name index's matching, which is a specification rather than an
--   implementation detail: the index file is public, and a client that
--   reads it -- the Emacs mode, or one written in another language --
--   has to rank the same way this does.  So the ranks are pinned here
--   by example as well as by property.

module Internal.Interaction.NameIndex ( tests ) where

import Data.Aeson              ( Value(String), encode )
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Maybe              ( isJust, isNothing )
import Data.Text               ( Text )
import Data.Text qualified as T

import Agda.Interaction.NameIndex

import Internal.Helpers

------------------------------------------------------------------------
-- * Generators

-- | A name-shaped string: letters, digits and the two characters the
--   third rank splits on, plus one non-ASCII character, since Agda
--   names are not ASCII and the byte-level prefilter has to cope.
nameChar :: Gen Char
nameChar = elements $ ['a'..'f'] ++ ['0'..'2'] ++ "-_ℤ"

nameText :: Gen Text
nameText = T.pack <$> listOf nameChar

nonEmptyNameText :: Gen Text
nonEmptyNameText = T.pack <$> listOf1 nameChar

-- | Rank a literal pattern against a name.
lit :: Text -> Text -> Maybe MatchRank
lit p = rankOf (compilePattern False p)

-- | Rank a glob pattern against a name.
glob :: Text -> Text -> Maybe MatchRank
glob p = rankOf (compilePattern True p)

------------------------------------------------------------------------
-- * The ranks, by example

-- | The table a second implementation has to reproduce.
prop_ranks :: Property
prop_ranks = conjoin
  [ counterexample (show (p, n, expected)) $ lit p n === expected
  | (p, n, expected) <-
      [ ("is-basis"    , "is-basis"          , Just ExactMatch)
      , ("is-basis"    , "is-basis-for"      , Just PrefixMatch)
      , ("basis"       , "is-basis-for"      , Just ComponentMatch)
      , ("basi"        , "is-basis-for"      , Just SubstringMatch)
      , ("is_basis"    , "is_basis"          , Just ExactMatch)
      , ("basis"       , "is_basis_for"      , Just ComponentMatch)
      , ("basis"       , "is-basisₛ-for"     , Just SubstringMatch)
      , ("Basis"       , "is-basis"          , Nothing)
      , ("sisab"       , "is-basis"          , Nothing)
      , ("ℤ*-assoc"    , "ℤ*-assoc"          , Just ExactMatch)
      , ("ℤ*-assoc"    , "ℤ*-assoc₀"         , Just PrefixMatch)
      , ("*"           , "ℤ*-assoc"          , Just SubstringMatch)
      ]
  ]

-- | Globbing is anchored piece by piece, but ranked the same way, so
--   that a glob with no wildcard in it behaves like the literal it is.
prop_globRanks :: Property
prop_globRanks = conjoin
  [ counterexample (show (p, n, expected)) $ glob p n === expected
  | (p, n, expected) <-
      [ ("is-*-for"    , "is-basis-for"      , Just ExactMatch)
      , ("is-*"        , "is-basis-for"      , Just ExactMatch)
      , ("*-for"       , "is-basis-for"      , Just ExactMatch)
      , ("is-?asis"    , "is-basis"          , Just ExactMatch)
      , ("is-?asis"    , "is-basis-for"      , Just PrefixMatch)
      , ("basi?"       , "is-basis-for"      , Just ComponentMatch)
      , ("asi?"        , "is-basis-for"      , Just SubstringMatch)
      , ("is-*-for"    , "is-basis"          , Nothing)
        -- A literal star is written with a backslash, which is the
        -- whole reason globbing is opt-in: Agda names contain stars.
      , ("ℤ\\*-*"      , "ℤ*-assoc"          , Just ExactMatch)
      , ("ℤ*-*"        , "ℤ-assoc"           , Just ExactMatch)
      ]
  ]

-- | A pattern with no wildcards in it ranks as the literal it is.
prop_globNoWildcards :: Property
prop_globNoWildcards =
  forAll nameText $ \ p ->
  forAll nameText $ \ n ->
  glob p n === lit p n

------------------------------------------------------------------------
-- * The ranks, by property

prop_exactIsFirst :: Property
prop_exactIsFirst = forAll nonEmptyNameText $ \ n -> lit n n === Just ExactMatch

prop_properPrefixIsSecond :: Property
prop_properPrefixIsSecond =
  forAll nonEmptyNameText $ \ p ->
  forAll nonEmptyNameText $ \ suffix ->
  lit p (p <> suffix) === Just PrefixMatch

-- | Every rank is a substring match, so a pattern that does not occur
--   in the name does not match it at all.  This is what makes the
--   byte-level prefilter of 'queryIndex' sound.
prop_matchIsSubstring :: Property
prop_matchIsSubstring =
  forAll nonEmptyNameText $ \ p ->
  forAll nameText $ \ n ->
  isJust (lit p n) === (p `T.isInfixOf` n)

-- | A component is delimited by a hyphen or an underscore at each end.
prop_components :: Property
prop_components = conjoin
  [ nameComponents "is-basis-for" === ["is", "basis", "for"]
  , nameComponents "is_basis"     === ["is", "basis"]
  , nameComponents "-"            === []
  , nameComponents "x"            === ["x"]
  ]

-- | The components of a name are pieces of it, and rejoining them with
--   a separator gives back a name with the same components.
prop_componentsArePieces :: Property
prop_componentsArePieces =
  forAll nameText $ \ n ->
  conjoin [ counterexample (T.unpack c) $ property (c `T.isInfixOf` n)
          | c <- nameComponents n ]

------------------------------------------------------------------------
-- * The filters

prop_kinds :: Property
prop_kinds = conjoin
  [ property     $ matchesKind [] "FunName"
  , property     $ matchesKind ["FunName"] "FunName"
  , property     $ matchesKind ["funname"] "FunName"
  , property     $ matchesKind ["Fun"] "FunName"
  , property     $ matchesKind ["fun"] "FunName"
  , property     $ matchesKind ["rec", "fun"] "FunName"
  , property     $ not $ matchesKind ["Rec"] "FunName"
  , property     $ not $ matchesKind ["Function"] "FunName"
  ]

-- | The type filter is @search@'s predicate over the qualified names a
--   type mentions: all of the terms, each occurring in some name.
prop_mentions :: Property
prop_mentions = conjoin
  [ property     $ mentionsAll [] []
  , property     $ mentionsAll ["Sup"] ["OrderedTypes.SupLattice.Sup-Lattice"]
  , property     $ mentionsAll ["UF."] ["UF.Base.ap"]
  , property     $ mentionsAll ["ap", "UF"] ["UF.Base.ap"]
  , property     $ not $ mentionsAll ["ap", "Id"] ["UF.Base.ap"]
  ]

-- | Rank 5 looks at the unqualified part of a mentioned name, which is
--   what one would have typed.
prop_mentionRank :: Property
prop_mentionRank = conjoin
  [ mentionRank (compilePattern False "Sup-Lattice")
      ["OrderedTypes.SupLattice.Sup-Lattice"] === Just MentionMatch
  , mentionRank (compilePattern False "SupLattice")
      ["OrderedTypes.SupLattice.Sup-Lattice"] === Nothing
  , property $ isNothing $ mentionRank (compilePattern False "x") []
  ]

------------------------------------------------------------------------
-- * The prefilter

-- | 'queryIndex' skips a line of the index whose raw bytes do not
--   contain the pattern.  That is only sound because JSON escapes a
--   string character by character, so the encoding of a substring is a
--   substring of the encoding -- whatever the encoder does with quotes,
--   backslashes and characters outside the basic plane.
prop_needleIsSound :: Property
prop_needleIsSound =
  forAll arbitrary $ \ (before, needle, after) ->
    let whole = T.pack before <> T.pack needle <> T.pack after
    in  property $ jsonNeedle (T.pack needle)
          `B.isInfixOf` BS.toStrict (encode (String whole))

------------------------------------------------------------------------
-- * All tests
------------------------------------------------------------------------

-- Template Haskell hack to make the following $allProperties work
-- under ghc-7.8.
return [] -- KEEP!

tests :: TestTree
tests = testProperties "Internal.Interaction.NameIndex" $allProperties
