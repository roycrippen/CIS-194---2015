{-# OPTIONS_GHC -Wall #-}
--{-# LANGUAGE BangPatterns #-}

module Hw6 where
import Data.List

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
rand = sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------
{- Total Memory in use: 91 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------
{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = go 9999999999 0
    where go mn mx [] = Just (mn, mx)
          go mn mx (x:xs)
              | x < mn = go x mx xs
              | x > mx = go mn x xs
              | otherwise = go mn mx xs

-- Exercise 10 ----------------------------------------
newtype Matrix a = M [[a]] deriving (Eq, Show)

mFib :: Matrix Integer
mFib = M [[1,1], [1,0]]

instance Num a => Num (Matrix a) where
    M a * M b = M [ [ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]
    M a + M b = M (zipWith (zipWith (+)) a b)
    M a - M b = M (zipWith (zipWith (-)) a b)
    negate (M a) = M (map (map negate) a)
    fromInteger x = M (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1

fastFib :: Int -> Integer
fastFib n = head . head $ matrix
    where (M matrix) = mFib^n
