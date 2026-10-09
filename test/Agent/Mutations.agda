-- Material for the mutating methods of the agent frontend: enough
-- holes, written both ways, to drive the file to a complete proof.

module Mutations where

infixl 20 _+_
infix  10 _≡_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

cong : {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

data Pair (A B : Set) : Set where
  _,_ : A → B → Pair A B

-- The second hole sits in an argument position, so what is given has
-- to be parenthesised on the way in.
+-idʳ : (n : ℕ) → n + zero ≡ n
+-idʳ zero    = {!!}
+-idʳ (suc n) = cong suc {!!}

-- Refining this one leaves a goal per argument of the constructor.
p : Pair ℕ ℕ
p = ?

-- Introduction turns this one into a lambda.
idℕ : ℕ → ℕ
idℕ = {!!}

-- An expression spanning several lines is indented to the hole.
four : ℕ
four = {!!}

-- The type checker solves this one by itself.
ex : (n : ℕ) → n ≡ n
ex n = refl {x = {!!}}
