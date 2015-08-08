module Main where

import Hw6
import Testing
import Hw6Tests
import Test.QuickCheck


main :: IO()
main = do
    putStrLn "running unit tests..."
    let a = runTests allTests
    if null a
        then putStrLn "all unit tests ok"
        else print a
    putStrLn "running property tests..."
    quickCheck prop_Fibonacci1
    quickCheck prop_Fibonacci2
    print ("fib 24 = 75025", fib 24)
    print ("first 25 fibs1 = ", take 25 fibs1)
    print ("first 25 fibs2 = ", take 25 fibs2)
    putStrLn "done..."
