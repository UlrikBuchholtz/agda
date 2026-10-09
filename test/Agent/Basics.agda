-- Basic material for the agent frontend tests: a proof with holes,
-- Unicode names, and enough structure for goals with a context.

module Basics where

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

+-idʳ : (n : ℕ) → n + zero ≡ n
+-idʳ zero    = refl
+-idʳ (suc n) = {!!}
