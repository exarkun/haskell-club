module Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate
    ) where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (n `mod` 10):(toDigitsRev (n `div` 10))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x:y*2:doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x >= 10    = (sumDigits . toDigits) x + sumDigits xs
    | otherwise  = x + sumDigits xs

validate :: Integer -> Bool
validate =
    -- (sumDigits (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0
    -- (sumDigits . doubleEveryOther . toDigitsRev) n `mod` 10 == 0
    -- ((flip mod 10) . sumDigits . doubleEveryOther . toDigitsRev) n == 0
    ((==) 0 . (flip mod 10) . sumDigits . doubleEveryOther . toDigitsRev)
