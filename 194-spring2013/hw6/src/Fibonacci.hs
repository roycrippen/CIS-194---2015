{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

import Data.List (intercalate, transpose)

-- exercise 1 ----------------------------------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2 ----------------------------------
fibs2 :: [Integer]
fibs2 = 0 : 1 : go 0 1
  where go a b = (a + b) : go b (a + b)

-- exercise 3 ----------------------------------
data Stream a = a `Cons` (Stream a)

streamToList :: Stream a -> [a]
streamToList (a `Cons` xs) =  a : streamToList xs

instance Show a => Show (Stream a) where
    show a = "[" ++ intercalate "," (map show $ take 20 $ streamToList a) ++ ",...]"

-- exercise 4 ----------------------------------
streamRepeat :: a -> Stream a
streamRepeat a = a `Cons` streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a `Cons` xs) = f a `Cons` streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a `Cons` streamFromSeed f (f a)

-- exercise 5 ----------------------------------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a `Cons` xs) ys = a `Cons` interleaveStreams ys xs

ruler :: Stream Integer
ruler = weave (streamMap streamRepeat nats)
    where weave (a `Cons` xs) = interleaveStreams a (weave xs)

ruler' :: Stream Integer
ruler' = go 0
    where go a = interleaveStreams (streamRepeat a) (go (a + 1))

-- to test any ruler function
-- streamToList ruler !! (2^n - 1) should be == n
-- anything over 25 gets very slow
-- speed of ruler and ruler' are virtually the same

-- exercise 6 ----------------------------------
x :: Stream Integer
x = 0 `Cons` (1 `Cons` streamRepeat 0)

instance Num (Stream Integer) where
    fromInteger n = n `Cons` streamRepeat 0
    negate = streamMap negate
    (+) (a `Cons` as) (b `Cons` bs) = (a + b) `Cons` (as + bs)
    (*) (a `Cons` as) b'@(b `Cons` bs) = (a * b) `Cons` (streamMap (*a) bs + (as * b'))

instance Fractional (Stream Integer) where
    (/) a'@(a `Cons` as) b'@(b `Cons` bs) = (a `div` b) `Cons` streamMap (`div` b) (as - (a' / b') * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)
-- very interesting but extremely slow

-- exercise 7 ----------------------------------
newtype Matrix a = M [[a]] deriving (Eq, Show)

mFib :: Matrix Integer
mFib = M [[1,1], [1,0]]

instance Num a => Num (Matrix a) where
    M a * M b = M [ [ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]

fib4 :: Int -> Integer
fib4 n | n <= 0 = 0
fib4 n = head . head $ matrix
    where (M matrix) = mFib^n
-- very fast
