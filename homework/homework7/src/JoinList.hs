module JoinList where

import Sized
import Scrabble

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append (tag left <> tag right) left right

-- exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ left right)
  | i < 0         = Nothing
  | i < leftSize  = indexJ i left
  | otherwise     = indexJ (i - leftSize) right
  where
    leftSize = getSize . size . tag $ left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n v@(Single m a)
  | n <= 0 = v
  | n > 0  = Empty
dropJ n jl@(Append _ left right)
  | n <= 0        = jl
  | n  < leftSize = (dropJ n left) +++ right
  | n == leftSize = right
  | n  > leftSize = dropJ (n - leftSize) right
  where
    leftSize = getSize . size . tag $ left

dropJ _ Empty = Empty
dropJ _ s@(Single _ _) = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 jl = Empty
takeJ n Empty = Empty
takeJ n s@(Single _ _) = s
takeJ n jl@(Append _ left right)
  | n  < 0        = Empty
  | n  < leftSize = takeJ n left
  | n == leftSize = left
  | n  > leftSize = left +++ takeJ (n - leftSize) right
  where
    leftSize = getSize . size . tag $ left

-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line

scoreAndSizeLine :: String -> JoinList (Score, Size) String
scoreAndSizeLine line = Single (scoreString line, Size 1) line
