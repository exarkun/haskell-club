module Main where

import Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    )

import Hanoi
    ( hanoi
    )

main :: IO ()
main = do
    putStrLn $ show $ toDigits 0
    putStrLn $ show $ toDigits (-17)
    putStrLn $ show $ toDigits 1234
    putStrLn $ show $ toDigitsRev 1234

    putStrLn $ show $ doubleEveryOther []
    putStrLn $ show $ doubleEveryOther [1]
    putStrLn $ show $ doubleEveryOther [1,1,1]
    putStrLn $ show $ doubleEveryOther [1,1,1,1]

    putStrLn $ show $ sumDigits [1, 2, 3]
    putStrLn $ show $ sumDigits [10, 2, 3]
    putStrLn $ show $ sumDigits [15, 25, 35]

    putStrLn $ show $ validate 4012888888881881
    putStrLn $ show $ validate 4012888888881882

    putStrLn $ show $ hanoi 2 "a" "b" "c"
    putStrLn $ show $ hanoi 5 "a" "b" "c"
