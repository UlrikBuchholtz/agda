-- The file the session loads.  It imports nothing else in the library:
-- what the build does with the other two modules is not this file's
-- business.

module Lib.Main where

identity : Set → Set
identity A = A
