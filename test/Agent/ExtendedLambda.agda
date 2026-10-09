-- A scope error inside a pattern lambda.
--
-- Agda scope checks such a lambda as a synthetic declaration whose
-- unused fields are deliberate `__IMPOSSIBLE__` placeholders, so the
-- call it is checked under is one that cannot be printed.  The agent
-- frontend prints every enclosing call, which used to force those
-- placeholders and take the whole process down -- losing the scope
-- error, which is the one thing the client needed.

module ExtendedLambda where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

f : Nat → Nat
f = λ where zero    → nosuchname
            (suc n) → n
