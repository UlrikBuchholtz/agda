-- Material for the profiling test: one definition that is slow to
-- check, and one whose check does not terminate.

{-# OPTIONS --no-termination-check #-}
module Profile where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

data Bool : Set where
  true false : Bool

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

_+_ : ℕ → ℕ → ℕ
zero  + n = n
suc m + n = suc (m + n)

fib : ℕ → ℕ
fib zero          = zero
fib (suc zero)    = suc zero
fib (suc (suc n)) = fib n + fib (suc n)

even : ℕ → Bool
even zero          = true
even (suc zero)    = false
even (suc (suc n)) = even n

four : ℕ
four = suc (suc (suc (suc zero)))

twenty-three : ℕ
twenty-three = four + (four + (four + (four + (four + suc (suc (suc zero))))))

-- fib 23 = 28657: computing it in unary takes a while, and the answer
-- is short, so the time goes to this definition and not to parsing.
slow : even (fib twenty-three) ≡ false
slow = refl

loop : ℕ → ℕ
loop n = loop (suc n)

spin : loop zero ≡ zero
spin = refl
