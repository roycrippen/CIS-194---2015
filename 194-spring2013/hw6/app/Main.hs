module Main where

import           Fibonacci

main :: IO ()
main = do
    putStrLn "CIS 194, Spring 2013 assignment 6"
    putStrLn ""
    print "take 26 fibs1"
    print (take 26 fibs1)
    print ("take 30 fibs1 == take 30 fibs2", take 30 fibs1 == take 30 fibs2)
    let n = 2^(20 :: Int) - 1
    print ("streamToList ruler !! (2^20 - 1) == ", streamToList ruler !! n)
    print ("streamToList fibs3 !! 1000", streamToList fibs3 !! 25)
    putStrLn "\ndone..."
