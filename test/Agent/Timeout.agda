-- Material for the per-request timeout test.

{-# OPTIONS --no-termination-check #-}
module Timeout where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

loop : ℕ → ℕ
loop n = loop (suc n)

goal : ℕ
goal = {!!}
