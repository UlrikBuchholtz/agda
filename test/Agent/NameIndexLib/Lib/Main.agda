-- The file the session loads.  It imports nothing else in the library,
-- so every hit from Lib.Basis is a hit in a module the loaded file does
-- not import -- which is what this whole feature is for.

module Lib.Main where

identity : Set → Set
identity A = A
