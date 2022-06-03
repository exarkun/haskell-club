{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc
  ( eval
  , compile
  , evalStr
  , toInfixString
  , withVars
  , Expr(lit, add, mul)
  , MinMax(MinMax)
  , Mod7(Mod7)
  ) where

import qualified Data.Map as M

import ExprT
  ( ExprT(Lit, Add, Mul)
  )

import qualified VarExprT

import qualified StackVM

import Parser
  ( parseExp
  )

import qualified StackVM

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

-- Exercise 3
-- Create a type class called Expr with three methods called lit, add,
-- and mul which parallel the constructors of ExprT. Make an instance of
-- Expr for the ExprT type, in such a way that
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4
-- Make instances of Expr for each of the following types:
-- ...
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

-- Exercise 5
-- For any arithmetic expression exp :: Expr a => a it should be the case that
-- stackVM exp == Right [IVal exp]

instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  add a b = (a ++ b) ++ [StackVM.Add]
  mul a b = (a ++ b) ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Exercise 6 - representation and evaluation of expressions with variables

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


instance Expr VarExprT.VarExprT where
  lit = VarExprT.Lit
  add = VarExprT.Add
  mul = VarExprT.Mul

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \vars -> Just n
  add left right = \vars -> (+) <$> (left vars) <*> (right vars)
  mul left right = \vars -> (*) <$> (left vars) <*> (right vars)

instance Expr StackVM.Stack where
  lit x = StackVM.PushI x
  add = StackVM.Add
  mul = StackVM

compile :: String -> Maybe Program
compile expr = parseExp Lit Add Mul expr
