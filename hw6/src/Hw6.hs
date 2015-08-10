{-# OPTIONS_GHC -Wall #-}
--{-# LANGUAGE BangPatterns #-}
module Hw6 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [ fib n | n <- [0..] ]

-- Exercise 2 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------
data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate ur seed = Cons seed (sIterate ur (ur seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------
nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleaveList (streamToList (fmap sRepeat nats))
    where sInterleaveList ~(x:xs) = sInterleave x (sInterleaveList xs)

-- Exercise 7 -----------------------------------------
-- | Implementation of C rand
rand :: Int -> Stream Int
rand = undefined

-- Exercise 8 -----------------------------------------
{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------
{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

--main :: IO ()
--main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
fastFib :: Int -> Integer
fastFib = undefined
