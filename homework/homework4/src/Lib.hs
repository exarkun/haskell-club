module Lib
  ( fun1
  , fun1'
  , fun2
  , fun2'

  , foldTree
  , isBalanced
  , treeHeight
  , Tree(Leaf, Node)

  , xor
  , map'
  , myFoldl
  , sieveSundaram
  ) where

import Data.Bool.Unicode
  ( (⊻) -- boolean xor
  )

--
-- Exercise 1
--

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (foldr (*) 1) . (map (subtract 2)) . (filter even)

fun2 :: Integer -> Integer
fun2 0 = undefined
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum . takeWhile (>1) . iterate fun2''
  where
    fun2'' n
      | even n    = n `div` 2
      | otherwise = 3 * n + 1

-- Note that each node stores an extra Integer representing the pre-computed
-- height at that node.
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

--
-- Exercise 2
--

-- generates a balanced binary tree from a list of values using foldr
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) =
  let
    (leftElements, rightElements) = splitAt (length xs `div` 2) xs
    left = foldTree leftElements
    right = foldTree rightElements
    height = 1 + max (treeHeight left) (treeHeight right)
  in
    Node height left x right

-- The height of http://en.wikipedia.org/wiki/Binary_tree a binary tree is the
-- length of a path from the root to the deepest node. For example, the height
-- of a tree with a single node is 0; the height of a tree with three nodes,
-- whose root has two children, is 1; and so on.
treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node height _ _ _) = height

-- A binary tree is balanced if the height of its left and right subtrees
-- differ by no more than 1, and its left and right subtrees are also
-- balanced.
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node height left x right) =
  abs (treeHeight left - treeHeight right) <= 1 && isBalanced left && isBalanced right

instance Foldable Tree where
  -- The length of a tree is the number of values Nodes it contains.
  length Leaf = 0
  length (Node _ left x right) = 1 + length left + length right


--
-- Exercise 3
--

xor :: [Bool] -> Bool
xor = foldr (⊻) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> (f a):b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

--
-- Exercise 4
--

-- Given an integer n, your function should generate all the odd prime numbers
-- up to 2n + 2.
--
-- Start with a list of the integers from 1 to n. From this list, remove all
-- numbers of the form i + j + 2ij where:
--
--   i, j ∈ ℕ, 1 ≤ i ≤ j
--   i + j + 2ij ≤ n
--
--
-- The remaining numbers are doubled and incremented by one, giving a list of
-- the odd prime numbers (i.e., all primes except 2) below 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let
    starting = [1..n] :: [Integer]
    composites = [ i + j + i * j * 2
                 | j <- [1..n]
                 , i <- [1..j]
                 ]
    elem' = flip elem
    isComposite = elem' composites
    sieved = filter (not . isComposite) starting
  in
    map ((+1) . (2*)) sieved
