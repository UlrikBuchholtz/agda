-- Material for case splitting: a clause of every shape the agent has
-- to write over.  Splitting replaces the whole clause, so the shapes
-- that the Emacs mode's line replacement cannot handle -- a clause
-- spread over two lines, a clause of an inline extended lambda --
-- matter here.

module CaseSplit where

data Bool : Set where
  true false : Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

record Pair (A B : Set) : Set where
  field
    fst : A
    snd : B

-- A plain function clause.
not : Bool → Bool
not x = {!x!}

-- A clause with a where block, which the split must leave standing.
double : ℕ → ℕ
double m = {!m!}
  where
  twice : ℕ → ℕ
  twice n = n

-- A clause spread over two lines.
first : ℕ → ℕ → ℕ
first m
      n = {!m!}

-- A clause of an inline extended lambda, beside one that stays.
andTrue : Bool → Bool
andTrue = λ { true → true ; x → {!x!} }

-- A clause of a λ where, which stays on a line of its own.
negate : Bool → Bool
negate = λ where
  x → {!x!}

-- A with-clause.
isZero : ℕ → Bool
isZero m with m
... | x = {!x!}

-- Splitting on the result rather than on a variable.
pair : Pair Bool Bool
pair = {!!}
