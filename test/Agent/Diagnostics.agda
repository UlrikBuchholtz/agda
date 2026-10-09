-- Material for structured diagnostics: a warning Agda has a name for,
-- an error raised under a chain of calls, and a context whose entries
-- carry modalities.

{-# OPTIONS --erasure #-}

module Diagnostics where

data Bool : Set where
  true false : Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

-- The second clause is unreachable, which Agda warns about by name.
constTrue : Bool → Bool
constTrue x = true
constTrue true = false

-- Hidden, erased and irrelevant arguments, so that the context has a
-- modality to report.
modal : {A : Set} → @0 ℕ → .Bool → ℕ
modal n b = {!!}
