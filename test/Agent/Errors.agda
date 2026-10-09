-- Material for the agent frontend's error paths.

module Errors where

data Bool : Set where
  true false : Bool

not : Bool → Bool
not true  = false
not false = {!!}
