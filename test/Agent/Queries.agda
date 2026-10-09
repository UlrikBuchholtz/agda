-- Material for the query methods: names to search for, a module to
-- list, a name to explain, and a goal to ask about.

module Queries where

infixl 20 _+_
infix  10 _≡_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

module Nested where

  double : ℕ → ℕ
  double n = n + n

open Nested

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

lemma : (n : ℕ) → double n ≡ n + n
lemma n = {!!}
