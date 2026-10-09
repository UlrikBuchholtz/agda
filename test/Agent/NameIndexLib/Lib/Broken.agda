-- This module does not type-check, so nothing ever writes an interface
-- for it: it is `unindexed` rather than silently absent, and the answer
-- to it is to check it, which no rebuild of the index would do.

module Lib.Broken where

open import Lib.Basis

wrong : Bool
wrong = Set
