module Main where

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
