module BoolExprT
  ( T(Lit, Add, Mul)
  , eval
  ) where

import Calc
  ( Expr(lit, add, mul)
  )

data T
  = Lit Bool
  | Add T T
  | Mul T T
  deriving (Show, Eq)

instance Expr T where
  lit = \x -> Lit (x > 0)
  add = Add
  mul = Mul

eval :: T -> Bool
eval (Lit x) = x
eval (Add x y) = eval x || eval y
eval (Mul x y) = eval x && eval y
