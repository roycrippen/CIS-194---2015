module Main where

import           Hw1

l :: Integer
l = 5594589764218858

main :: IO()
main = do
    putStrLn "CIS 194, Spring 2015 assignment 1"
    putStrLn ""
    putStrLn $ "luhn of " ++ show l ++ " is " ++ show (luhn l)
    putStrLn $ "luhn of " ++ show (l+1) ++ " is " ++ show (luhn (l+1))
    putStrLn ""
    putStrLn "hanoi 3: "
    print $ hanoi 3 "from" "to" "helper"
