module BoolExprT
  ( T(Lit, Add, Mul)
  ) where

import Calc
  ( Expr(lit, add, mul)
  )

data T
  = Lit Integer
  | Add T T
  | Mul T T
  deriving (Show, Eq)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)
