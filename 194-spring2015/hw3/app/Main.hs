module Main where

import           Hw3


main :: IO()
main = do
    putStrLn "CIS 194, Spring 2015 assignment 3"
    putStrLn ""
    let fact8 = state (run (extend empty "In" 8) factorial) "Out"  -- out = 40,320
    print ("8! = 40320", fact8)
    let sqrt65 = state (run (extend empty "A" 65) squareRoot) "B"  -- B = 8
    print ("sqrt 65 = 8", sqrt65)
    let fib25 = state (run (extend empty "In" 25) fibonacci) "Out"  -- Out = 75,025
    print ("fib 25 = 75025", fib25)
    putStrLn "done..."
