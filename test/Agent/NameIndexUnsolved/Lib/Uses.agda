-- The module that imports the one above, and so has it checked a second
-- time.  Under plain `agda` this fails with the unsolved metas of
-- Lib.Unsolved, and the builder records exactly that.

module Lib.Uses where

open import Lib.Unsolved public

twice : Nat → Nat
twice = double
