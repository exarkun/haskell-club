module SpecExpr
  ( prop_lit
  , prop_add
  , prop_mul
  , spec_assignmentExample
  , prop_evals
  , prop_roundtrip
  , prop_roundtrip'
  , spec_roundtrips
  ) where

import Calc
  ( eval
  , evalStr
  , toInfixString
  )
import ExprT
  ( ExprT(Lit, Add, Mul)
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
