import Test.Hspec
  ( hspec
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )
import Test.QuickCheck
  ( Gen
  , forAll
  , property
  , arbitrary
  , scale
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

import Data.List.Utils
  ( countElem
  )

import Lib
  ( fun1
  , fun1'
  , fun2
  , fun2'

  , foldTree
  , Tree(Leaf, Node)
  , treeHeight
  , isBalanced

  , xor
  , map'
  , myFoldl
  , sieveSundaram
  )


positiveIntegers :: Gen Integer
positiveIntegers = do
  n <- (arbitrary :: Gen Integer)
  (return . (+1) . abs) n

main :: IO ()
main = hspec $ do
  describe "fun1'" $
    it "behaves exactly like fun1" $
      property $ \xs ->
        fun1' xs `shouldBe` fun1 xs

  describe "fun2'" $
    it "behaves exactly like fun2" $
      -- Keep the values small.  fun2 is expensive to compute for even
      -- trivially large inputs.
      forAll positiveIntegers $ \x ->
        fun2' x `shouldBe` fun2 x

  describe "xor" $
    it "returns True if and only if the given list contains an odd number of True" $
      property $ \xs ->
        xor xs `shouldBe` ((== 1) . (flip mod 2) . (countElem True)) xs

  describe "map'" $
    it "behaves exactly like map" $ do
      property $ \xs ->
        (map' (*2) (xs :: [Integer])) `shouldBe` (map (*2) xs)

  describe "myFoldl" $
    it "behaves exactly like foldl" $
    do
      let f = (-) :: (Int -> Int -> Int)
      property $ \xs x ->
        myFoldl f x xs `shouldBe` foldl f x xs

  describe "sieveSundaram" $ do
    let prime i = all ((/= 0) . (i `mod`)) [2..(i `div` 2)]
    it "returns a list of numbers less than or equal to 2n + 2" $ do
      property $ \n ->
        sieveSundaram n `shouldSatisfy` (all (<=2 * n + 2))

    it "returns a list of only prime numbers" $ do
      property $ \n ->
        sieveSundaram n `shouldSatisfy` all prime

    it "returns all the the odd prime numbers less than or equal to 2n + 2" $ do
      property $ \n ->
        sieveSundaram n `shouldBe` filter prime [3, 5..2 * n + 1]

  describe "foldTree" $ do
    it "returns a balanced tree" $
      property $ \xs ->
        (isBalanced . foldTree) (xs :: [Char]) `shouldBe` True

    it "returns a tree of the same size as the input" $
      property $ \xs ->
        (length . foldTree) (xs :: [Char]) `shouldBe` length xs

    it "returns a tree where each node's height is greater than that of its children" $
      property $ \xs ->
        let tree = foldTree (xs :: [Char])
        in heightInvariant tree `shouldBe` True
        where
          heightInvariant :: Tree a -> Bool
          heightInvariant Leaf = True
          heightInvariant (Node height left _ right) =
            height > treeHeight left &&
            height > treeHeight right &&
            heightInvariant left &&
            heightInvariant right
