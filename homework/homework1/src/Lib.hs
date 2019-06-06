module Lib
    ( toDigits
    , toDigitsRev
    , doubleEveryOther
    , doubleEveryOther'
    , doubleEveryOther''
    , sumDigits
    , validate
    ) where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (n `mod` 10):(toDigitsRev (n `div` 10))

-- wrong implementation
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x:y*2:doubleEveryOther zs

-- corrected implementation
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' (x:[]) = [x]
doubleEveryOther' many = reverse . doubleEveryOther . reverse $ many

-- Less reverse-y implementation
doubleEveryOther'' :: [Integer] -> [Integer]
doubleEveryOther'' many
  | even . length $ many = applyEvery (2*) id many
  | odd . length $ many  = applyEvery id (2*) many
  where
    applyEvery :: (Integer -> Integer) -> (Integer -> Integer) -> [Integer] -> [Integer]
    applyEvery _ _ []            = []
    applyEvery _ _ (x:[])        = [x]
    applyEvery first second (x:y:rest) = [first x, second y] ++ applyEvery first second rest

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
    ((==) 0 . (flip mod 10) . sumDigits . doubleEveryOther'' . toDigits)
