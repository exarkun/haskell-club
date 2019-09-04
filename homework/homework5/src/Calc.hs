module Calc
  ( eval
  , evalStr
  , toInfixString
  , Expr(lit, add, mul)
  , MinMax(MinMax)
  , Mod7(Mod7)
  ) where

import ExprT
  ( ExprT(Lit, Add, Mul)
  )

import Parser
  ( parseExp
  )

-- Exercise 1
-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- Exercise 2
-- evaluates arithmetic expressions given as a String, producing Nothing for
-- inputs which are not well-formed expressions, and Just n for well-formed
-- inputs that evaluate to n.
evalStr :: String -> Maybe Integer
evalStr expr = eval <$> parseExp Lit Add Mul expr

toInfixString :: ExprT -> String
toInfixString (Lit n) = show n
toInfixString (Add x y) = "(" ++ toInfixString x ++ " + " ++ toInfixString y ++ ")"
toInfixString (Mul x y) = "(" ++ toInfixString x ++ " * " ++ toInfixString y ++ ")"

class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ min x y
  mul (MinMax x) (MinMax y) = MinMax $ max x y

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
