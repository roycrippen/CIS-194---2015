module Main where

import           Control.Monad.Random (evalRandIO)
import           Data.Vector          (Vector)
import qualified Data.Vector          as V
import           Hw7

testV :: Rnd (Vector Integer)
testV = randomVec 10000

qsortRandomTest :: Ord a => Rnd (Vector a) -> Rnd Bool
qsortRandomTest l = do
    l' <- l
    a <- qsortR l'
    b <- (qsortR . V.reverse) a
    return (a == b)

main :: IO()
main = do
    putStrLn "Homework 7 main..."
    putStrLn "random int vector 0 to 1000, length 10, 2 times..."
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    putStrLn "random lower case string vector, length 30, 2 times..."
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    putStrLn "shuffle [0..29] 2 times..."
    print =<< evalRandIO (shuffle (V.fromList [0..29]:: Vector Int))
    print =<< evalRandIO (shuffle (V.fromList [0..29]:: Vector Int))
    putStr "10K randoms Integers; qsortR == (qsortR . V.reverse) = "
    print =<< evalRandIO (qsortRandomTest testV)
    putStr "select rank 5K item from 10K sorted vector = "
    print =<< evalRandIO (select 5000 (V.fromList $ reverse [1..10000]::Vector Int))
    putStrLn "done...\n"
    evalRandIO newDeck >>= repl . State 100
