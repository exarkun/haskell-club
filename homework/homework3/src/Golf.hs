module Golf
  ( skips
  , localMaxima
  , histogram
  ) where

-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should contain
-- every second element from the input list. . . and the nth list in the
-- output should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips xs = xs:map (nth xs 0) [2..length xs]

nth :: [a] -> Int -> Int -> [a]
nth [] _ _ = []
nth (x:xs) position every =
  if position `mod` every == position then
    [x]
  else
    []
  ++ nth xs (position + 1) every

-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since it is
-- greater than the elements immediately before and after it (3 and 1). 5 is
-- not a local maximum since there is no element that comes after it
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (a:[]) = []
localMaxima (a:b:[]) = []
localMaxima (a:b:c:xs) =
  if b > a && b > c then
    [b]
  else
    []
  ++ localMaxima ([b, c] ++ xs)

-- takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers).
histogram :: [Integer] -> String
histogram _ = ""
