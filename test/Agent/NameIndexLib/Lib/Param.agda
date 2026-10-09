-- A parameterised module: its telescope is rendered once, as `params`,
-- rather than into the type of every name it defines.

module Lib.Param (A : Set) (B : Set) where

open import Lib.Basis

basis-of : Set
basis-of = A → B → Bool
