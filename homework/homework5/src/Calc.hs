module Calc
  ( eval
  ) where

import ExprT
  ( ExprT(Lit, Add, Mul)
  )

-- Exercise 1
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
