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

    it "returns a list where the nth is a list containing every nth element of the input" $ do
      property $ \xs ->
        let
          -- Compute the function
          result = skips xs
          -- Annotate with position to make assertions easier
          enumerated = indexed result
          check (index, element) =
            case index of
              0 -> element `shouldBe` xs
              n -> element `shouldBe` (filter ((== n + 1) . mod (n + 1)) xs)
        in
          map check enumerated
