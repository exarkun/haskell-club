module Main where

import Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , doubleEveryOther'
    , doubleEveryOther''
    , sumDigits
    , validate
    )

import Hanoi
    ( hanoi
    )

main :: IO ()
main = do
    putStrLn "toDigits"
    putStrLn $ show $ toDigits 0
    putStrLn $ show $ toDigits (-17)
    putStrLn $ show $ toDigits 1234
    putStrLn $ show $ toDigitsRev 1234

    putStrLn "doubleEveryOther"
    putStrLn $ show $ doubleEveryOther []
    putStrLn $ show $ doubleEveryOther [1]
    putStrLn $ show $ doubleEveryOther [1,1,1]
    putStrLn $ show $ doubleEveryOther [1,1,1,1]

    putStrLn "doubleEveryOther'"
    putStrLn $ show $ doubleEveryOther' []
    putStrLn $ show $ doubleEveryOther' [1]
    putStrLn $ show $ doubleEveryOther' [1,1,1]
    putStrLn $ show $ doubleEveryOther' [1,1,1,1]

    putStrLn "doubleEveryOther''"
    putStrLn $ show $ doubleEveryOther'' []
    putStrLn $ show $ doubleEveryOther'' [1]
    putStrLn $ show $ doubleEveryOther'' [1,1,1]
    putStrLn $ show $ doubleEveryOther'' [1,1,1,1]

    putStrLn "sumDigits"
    putStrLn $ show $ sumDigits [1, 2, 3]
    putStrLn $ show $ sumDigits [10, 2, 3]
    putStrLn $ show $ sumDigits [15, 25, 35]

    putStrLn "validate True"
    putStrLn $ show $ validate 0                  -- 0 == 0 mod 10
    putStrLn $ show $ validate 18                 -- 0 == (1 * 2) + 8 mod 10
    putStrLn $ show $ validate 26                 -- 0 == (2 * 2) + 6 mod 10
    putStrLn $ show $ validate 4012888888881881   -- ...
    putStrLn "validate False"
    putStrLn $ show $ validate 1                  -- 0 /= 1 mod 10
    putStrLn $ show $ validate 28                 -- 0 /= (2 * 2) + 8 mod 10
    putStrLn $ show $ validate 4012888888881882   -- ...

    putStrLn "hanoi"
    putStrLn $ show $ hanoi 2 "a" "b" "c"
    putStrLn $ show $ hanoi 5 "a" "b" "c"
