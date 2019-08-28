module Calc
  ( eval
  , evalStr
  , toInfixString
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
