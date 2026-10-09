-- A module that re-exports what Lib.Basis defines.  Its names are in
-- this module's scope, but they are not defined here, so the index must
-- find each of them once, under the module that does define it.

module Lib.Reexport where

open import Lib.Basis public

not-a-basis : Bool → Bool
not-a-basis true  = false
not-a-basis false = true
