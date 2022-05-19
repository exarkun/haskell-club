{-# LANGUAGE FlexibleInstances #-}

module VarExprT where

import qualified Data.Map as M

-- Exercise 6
-- To enable this, you first need to give arithmetic expressions the ability
-- to contain variables. Create a new type class HasVars a which contains a
-- single method var :: String -> a. Thus, types which are instances of
-- HasVars have some notion of named variables.

class HasVars a where
  var :: String -> a

data VarExprT = Var String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup
