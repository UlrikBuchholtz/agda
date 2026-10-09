-- Material for the ways a mutating method can decline to mutate: a
-- goal nothing fits, one several constructors fit, and one proof
-- search finds.

module MutationErrors where

infix 10 _≡_

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

data ⊥ : Set where

bad : ⊥
bad = {!!}

two : ℕ
two = {!!}

same : (n : ℕ) → n ≡ n
same n = {!!}

three : ℕ
three = {!!}
