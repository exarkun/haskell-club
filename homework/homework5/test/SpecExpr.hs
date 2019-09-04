module SpecExpr
  ( prop_lit
  , prop_add
  , prop_mul
  , spec_assignmentExample
  , prop_evals
  , prop_roundtrip
  , prop_roundtrip'
  , spec_roundtrips
  , spec_assignmentExample'
  , spec_boolExprT
  ) where

import Calc
  ( eval
  , evalStr
  , toInfixString
  , Expr(lit, add, mul)
  , testExp
  )
import ExprT
  ( ExprT(Lit, Add, Mul)
  )
import BoolExprT
  ( -- Get the Expr Bool instance
  )

import Parser
  ( parseExp
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldNotBe
  )

import Test.QuickCheck
  ( Arbitrary
  , Property
  , Gen
  , arbitrary
  , suchThatMap
  , frequency
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

instance Arbitrary ExprT where
  arbitrary = frequency
              [ (3, arbitrary `suchThatMap` (Just . Lit))
              , (1, arbitrary `suchThatMap` (\(x, y) -> Just $ Add x y))
              , (1, arbitrary `suchThatMap` (\(x, y) -> Just $ Mul x y))
              ]

prop_evals :: Property
prop_evals = property $ \x -> (evalStr . toInfixString $ x) `shouldNotBe` Nothing

prop_roundtrip :: Property
prop_roundtrip = property $ \x -> (=== Just x) . (parseExp Lit Add Mul) . toInfixString

prop_roundtrip' :: Property
prop_roundtrip' = property $
  \anExpr -> do
    let serialized = toInfixString anExpr
    let Just parsedExpr = parseExp Lit Add Mul serialized
    let reserialized = toInfixString parsedExpr
    serialized `shouldBe` reserialized

spec_roundtrips :: Spec
spec_roundtrips =
  let roundtrip x = toInfixString <$> parseExp Lit Add Mul x `shouldBe` Just x
  in
    describe "parseExp" $ do
    it "parses 0" $
      roundtrip "0"

    it "parses addition" $
      roundtrip "(0 + 0)"

    it "parses multiplication" $
      roundtrip "(0 * 0)"

    it "parses addition and multiplication" $
      roundtrip "((0 + 0) * 0)"

    it "parses ()" $
      roundtrip "((0 + 0) * 0)"

spec_assignmentExample' :: Spec
spec_assignmentExample' =
  describe "lit, add, and mul" $
  it "are equivalent to Lit, Add, Mul" $ do
  (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

spec_boolExprT :: Spec
spec_boolExprT = do
  describe "lit" $ do
    it "treats integers less than 0 as false" $
      testExp (lit (-1)) `shouldBe` False
    it "treats 0 as false" $
      testExp (lit 0) `shouldBe` False
    it "treats integers greater than zero as true" $
      testExp (lit 1) `shouldBe` True
  describe "add" $ do
    it "adding falses is false" $
      testExp (add (lit 0) (lit 0)) `shouldBe` False
    it "adding trues is true" $
      testExp (add (lit 1) (lit 1)) `shouldBe` True
    it "adding false and true is true" $
      testExp (add (lit 0) (lit 1)) `shouldBe` True
    it "adding true and false is true" $
      testExp (add (lit 1) (lit 0)) `shouldBe` True
  describe "mul" $ do
    it "multiplying falses is false" $
      testExp (mul (lit 0) (lit 0)) `shouldBe` False
    it "multiplying trues is true" $
      testExp (mul (lit 1) (lit 1)) `shouldBe` True
    it "multiplying false and true is false" $
      testExp (mul (lit 0) (lit 1)) `shouldBe` False
    it "multiplying true and false is true" $
      testExp (mul (lit 1) (lit 0)) `shouldBe` False

spec_parametricPolymorphism :: Spec
spec_parametricPolymorphism =
  let
    testExp :: Expr a => Maybe a
    testExp = parseExp lit add mul "(3 * -4) + 5"
  in do
    describe "Expr" $ do
      it "has Integer instance" $
        (testExp :: Maybe Integer) `shouldBe` Just -7
      it "has Bool instance" $
        (testExp :: Maybe Bool) `shouldBe` Just True
      it "has MinMax instance" $
        (testExp :: Maybe MinMax) `shouldBe` Just 5
      it "has Mod7 instance" $
        (testExp :: Maybe Mod7) `shouldBe` Just 0
