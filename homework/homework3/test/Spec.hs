import Test.Hspec
  ( hspec
  , describe
  , it
  , shouldBe
  )
import Test.QuickCheck
  ( Gen
  , forAll
  , property
  , arbitrary
  )
import Test.QuickCheck.Property
  ( failed
  , succeeded
  , reason
  )
import Control.Exception
  ( evaluate
  )

import Data.Maybe
  ( listToMaybe
  )
import Safe
  ( atMay
  )

import Data.List.Index
  ( indexed
  )

import Golf
  ( skips
  , localMaxima
  , histogram
  )

main :: IO ()
main = hspec $ do
  describe "skips" $ do
    it "returns a list with the input as its first element" $ do
      property $ \xs ->
        (listToMaybe (skips xs :: [[Integer]])) `shouldBe` Just xs

    it "returns a list with length equal to that of the input (or 1 for empty input)" $ do
      property $ \xs ->
        let
          result = skips xs :: [[Integer]]
          expectedLength = case length xs of
                             0 -> 1
                             n -> n
          gotLength = length result
        in
          gotLength `shouldBe` expectedLength

    it "returns a list of lists where the first element of all the lists combine to form the input" $ do
      forAll (arbitrary :: Gen (Integer, [Integer])) $ \(x, xs) ->
        let
          result = skips (x:xs) :: [[Integer]]
          firsts = sequence $ map listToMaybe result
        in
          firsts `shouldBe` Just (x:xs)

    it "returns a list where the nth element is a list containing every nth element of the input" $ do
      property $ \xs ->
        let
          result = skips xs :: [[String]]
          -- An element at position n is correct if all its elements at
          -- position e correspond to elements in the input at a position e
          -- times n.
          expected = xs:[ [ xs !! ((outer * inner) - 1)
                          | inner <- [1..length xs `div` outer]
                          , outer * inner <= length xs
                          ]
                        | outer <- [2..length xs]
                        ]
        in
          result `shouldBe` expected
