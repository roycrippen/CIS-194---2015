module Main where

import           Hw7
import           Control.Monad.Random (evalRandIO)
import           Data.Vector          (Vector, fromList)

main :: IO()
main = do
    putStrLn "Homework 6 main..."
    putStrLn $ "litfM' (+) Just 5 = " ++ show (liftM' (+1) (Just 5) :: Maybe Int)
    let swap = swapV 0 2 (fromList [1,2,3]) :: Maybe (Vector Int)
    putStrLn $ "swapV 0 2 (fromList [1,2,3]) = " ++ show swap
    putStr "randomElt = "
    print =<< ioVect (randomElt (fromList [0..9]:: Vector Int))
    putStrLn "random int vector 0 to 1000, length 10, 3 times..."
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    print =<< evalRandIO (randomVecR 10 (0, 1000) :: Rnd (Vector Int))
    putStrLn "random lower case string vector, length 30, 3 times..."
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    print =<< evalRandIO (randomVecR 30 ('a', 'z') :: Rnd (Vector Char))
    putStrLn "done..."
