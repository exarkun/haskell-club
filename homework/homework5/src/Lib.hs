module Lib
    ( id'
    , first
    , reverse'
    , reorder
    , compose
    , apply'
    ) where

-- The parametricity game.

id' :: a -> a
id' x = x

-- impossible :: a -> b
-- impossible x = ...

first :: a -> b -> a
first x y = x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reorder :: [a] -> [a]
reorder [] = []
reorder (x:xs) = if even $ length xs then x : xs else reorder xs ++ [x]

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = \x -> g (f x)

apply' :: (a -> a) -> a -> a
apply' f = \x -> f x
