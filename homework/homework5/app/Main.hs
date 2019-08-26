module Main where

import ExprT
import Calc

main :: IO ()
main = print $ eval (Lit 42)
