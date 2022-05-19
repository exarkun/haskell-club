module Main where

import VarExprT (HasVars(..), VarExprT)
import ExprT
import Calc
import StackVM

main :: IO ()
main = do
  print "eval (Lit 42)"
  print $ eval (Lit 42)
  print ""

  print "compiling..."
  let prog = compile "1 + 2 * (3 + 4) * 5 * (6 + 6)"
  print prog
  print "executing"
  print $ stackVM <$> prog
  print ""

  print $ Calc.withVars [("x", 6)] $ (lit 1)
  print $ Calc.withVars [("x", 6)] $ (add (lit 1) (lit 2))
  print $ Calc.withVars [("x", 6)] $ (mul (lit 2) (lit 6))
  print $ Calc.withVars [("x", 6)] $ (var "x")
  print $ Calc.withVars [("x", 6)] $ (add (var "x") (lit 1))
  print $ Calc.withVars [("x", 6)] $ (mul (var "x") (var "x"))
