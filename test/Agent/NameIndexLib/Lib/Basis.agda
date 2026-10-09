-- The pair of names the index exists to tell apart, plus a submodule,
-- whose names a caller reaches through a qualifier.

module Lib.Basis where

data Bool : Set where
  true false : Bool

is-basis : Set → Set
is-basis A = A → Bool

is-basis-for : Set → Set → Set
is-basis-for A B = A → B → Bool

module Sub where

  is-basis-sub : Set → Set
  is-basis-sub A = A → Bool
