-- Material for the project-wide name index: two names that differ only
-- in a suffix, which is the question the index exists to answer.  The
-- hole is there so that the module starts out unchecked, and so with no
-- interface for an index to read.

module NameIndex where

data Bool : Set where
  true false : Bool

is-basis : Set → Set
is-basis A = A → Bool

is-basis-for : Set → Set → Set
is-basis-for A B = A → {!!}
