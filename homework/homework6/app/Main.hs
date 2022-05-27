{-# LANGUAGE FlexibleInstances #-}

module Main where

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fibs2 :: [Integer]
fibs2 = 0:1:[ sum . take 2 . drop (n - 2) $ fibs2 | n <- [2..] ]

-- exercise 3
data Stream a = Stream a (Stream a)

instance (Show a) => Show (Stream a) where
  show stream = (show . take 20 . streamToList $ stream) ++ "..."

streamToList :: Stream a -> [a]
streamToList (Stream elem more) = elem:streamToList more

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat elem = Stream elem (streamRepeat elem)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream elem more) = Stream (f elem) (streamMap f more)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed =
  Stream seed (streamFromSeed f $ f seed)

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (1 +) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream elemA moreA) (Stream elemB moreB) =
  Stream elemA . Stream elemB $ interleaveStreams moreA moreB

ruler :: Stream Integer
ruler = undefined

-- exercise 6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger i = Stream i $ streamRepeat 0

  negate (Stream a0 a') = Stream (negate a0) (negate a')

  (Stream leftVal leftMore) + (Stream rightVal rightMore) =
    Stream (leftVal + rightVal) (leftMore + rightMore)

  (Stream a0 a') * b@(Stream b0 b') =
    Stream (a0 * b0) ((fromInteger a0) * b' + a' * b)

instance Fractional (Stream Integer) where
  (Stream a0 a') / (Stream b0 b') = q
    where
      q = Stream (a0 `div` b0) (a' - q * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- exercise 7

data Matrix a = Matrix a a a a

instance Num a => Num (Matrix a) where
  (Matrix a00 a10 a01 a11) * (Matrix b00 b10 b01 b11) =
    Matrix (dot a0 b0) (dot a0 b1) (dot a1 b0) (dot a1 b1)
    where
      a0 = [a00, a01]
      a1 = [a10, a11]
      b0 = [b00, b01]
      b1 = [b10, b11]

      dot [] [] = 0
      dot (a:as) (b:bs) = a * b + dot as bs

fib4 :: Integer -> Integer
fib4 n = r
  where
    f = Matrix 1 1 1 0 :: Matrix Integer
    (Matrix _ _ _ r) = f ^ n

main :: IO ()
main = do
  print "Fibs1"
  print $ take 20 fibs1
  print "Fibs2"
  print $ take 20 fibs2
  print "Naturals"
  print $ nats

  print "Generating function"
  print $ x ^ 4
  print $ (1 + x) ^ 5
  print $ (x ^ 2 + x + 3) * (x - 5)

  print "Fibs3"
  print $ fibs3
  print "Fibs4"
  print $ map fib4 [1..100]
