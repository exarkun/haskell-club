module Main where

import System.Environment
  ( getArgs
  )

import Data.List
  ( foldl'
  )

import Text.Printf
  ( printf
  )

import LogAnalysis
  ( parse
  , build
  , inOrder
  , whatWentWrong
  )
import Log
  ( LogMessage
  , testParse
  )

main :: IO ()
main = do
  argv <- getArgs
  msgs <- testParse parse 5000 $ argv !! 0
  putStrLn $ printf "Read %d messages" (length msgs)

  showMessages msgs
  showMessages . inOrder . build $ msgs
  putStrLn . joinLines . whatWentWrong $ msgs

joinLines :: [String] -> String
joinLines = foldl' (\a b -> a ++ "\n" ++ b) ""

showMessages :: [LogMessage] -> IO ()
showMessages msgs =
  putStrLn $ joinLines (map show msgs)
