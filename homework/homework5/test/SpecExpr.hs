module SpecExpr
  ( prop_lit
  , prop_add
  , prop_mul
  , spec_assignmentExample
  )where

import Calc
  ( eval
  )
import ExprT
  ( ExprT(Lit, Add, Mul)
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

import Test.QuickCheck
  ( Property
  , Gen
  , property
  , (===)
  )

prop_lit :: Property
prop_lit = property $ \x -> eval (Lit x) === x

prop_add :: Property
prop_add = property $ \x y -> eval (Add (Lit x) (Lit y)) === x + y

prop_mul :: Property
prop_mul = property $ \x y -> eval (Mul (Lit x) (Lit y)) === x * y

spec_assignmentExample :: Spec
spec_assignmentExample =
  describe "the assignment says that" $
  it "behaves in this one specific way" $
  eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
