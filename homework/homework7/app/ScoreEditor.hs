{-# LANGUAGE FlexibleInstances #-}
module Main where

import Editor
import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ left right) = toString left ++ "\n" ++ toString right

  fromString = foldl (+++) Empty . map scoreAndSizeLine . lines

  line = indexJ
  replaceLine n l b
    | n < 0 = b
    | n >= numLines b = b
    | otherwise = takeJ n b +++ (scoreAndSizeLine l) +++ dropJ (n + 1) b

  numLines b = size
    where
      (_, Size size) = tag b

  value b = score
    where
      (Score score, _) = tag b

main :: IO ()
main = runEditor editor buf
  where
    buf :: JoinList (Score, Size) String
    buf = fromString . unlines $
          [ "This buffer is for notes you don't want to save, and for"
          , "evaluation of steam valve coefficients."
          , "To load a different file, type the character L followed"
          , "by the name of the file."
          ]
