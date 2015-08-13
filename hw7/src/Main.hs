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
    putStrLn "Homework 6 main..."
    putStrLn $ "litfM' (+) Just 5 = " ++ show (liftM' (+1) (Just 5) :: Maybe Int)
    let swap = swapV 0 2 (V.fromList [1,2,3]) :: Maybe (Vector Int)
    putStrLn $ "swapV 0 2 (V.fromList [1,2,3]) = " ++ show swap
    putStr "randomElt = "
    print =<< evalRandIO (randomElt (V.fromList [0..9]:: Vector Int))
    putStrLn "random int vector 0 to 1000, length 10, 3 times..."
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    putStrLn "random lower case string vector, length 30, 3 times..."
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    putStrLn "shuffle [0..29] 3 times..."
    print =<< evalRandIO (shuffle (V.fromList [0..29]:: Vector Int))
    print =<< evalRandIO (shuffle (V.fromList [0..29]:: Vector Int))
    print =<< evalRandIO (shuffle (V.fromList [0..29]:: Vector Int))
    putStr "10K randoms Integers; qsortR == (qsortR . V.reverse) = "
    print =<< evalRandIO (qsortRandomTest testV)
    putStr "select rank 5K item from 10K sorted vector = "
    print =<< evalRandIO (select 5000 (V.fromList $ reverse [1..10000]::Vector Int))
    putStrLn "done..."
