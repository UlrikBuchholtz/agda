-- Material for the agent frontend's failed-load path: a goal, and a
-- definition after it that does not type-check.

module FailedLoad where

data Bool : Set where
  true false : Bool

not : Bool → Bool
not b = {!!}

wrong : Bool
wrong = not
