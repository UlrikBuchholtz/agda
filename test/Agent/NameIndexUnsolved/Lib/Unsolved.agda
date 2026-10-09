-- A module that type-checks but leaves a meta unsolved, so Agda writes
-- no interface for it.  That is what makes the module below check it a
-- second time, and the second check is what used to make the builder
-- die with an internal error rather than record the module.

module Lib.Unsolved where

postulate
  Nat : Set
  suc : Nat → Nat

unsolved : Nat → Nat
unsolved = _

double : Nat → Nat
double n = suc (suc n)
