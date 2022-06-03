module Main where

import JoinList
import Sized

main :: IO ()
main = do
  let x = Single (Size 1) "Hello, "
  let y = Single (Size 1) "Haskell!"
  let z = x +++ y
  print $ "indexJ"
  print $ indexJ 0 z
  print $ indexJ 1 z

  print $ "takeJ"
  print $ takeJ (-1) z
  print $ takeJ 0 z
  print $ takeJ 1 z
  print $ takeJ 2 z

  print $ "dropJ"
  print $ dropJ (-1) z
  print $ dropJ 0 z
  print $ dropJ 1 z
  print $ dropJ 2 z

  print $ "scrabble"
  print $ scoreLine "yay " +++ scoreLine "haskell!"
