module Calc
  ( eval
  , evalStr
  , toInfixString
  , Expr(lit, add, mul)
  , testExp
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

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = min x y
  mul (MinMax x) (MinMax y) = max x y

newtype Mod7 = Mod7 Integer

instance Expr Mod7 where
  lit x = x `mod` 7
  add x y = (x + y) `mod` 7
  mul x y = (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
