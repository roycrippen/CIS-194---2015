module Main where

import           Golf (histogram, localMaxima, skips)

main :: IO ()
main = do
    putStrLn "CIS 194, Spring 2013 assignment 3"
    putStrLn ""
    putStrLn $ "\n8th element of skips [1..64] = " ++ show (skips ([1..64]::[Int]) !! 7)
    putStrLn $ "\nlocalMaxima [2,9,5,6,1] == [9,6] = " ++ show (localMaxima [2,9,5,6,1] == [9,6])
    putStrLn "\nhistogram [1,4,5,4,6,6,3,4,2,4,9] = "
    putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
    putStrLn "\ndone..."
