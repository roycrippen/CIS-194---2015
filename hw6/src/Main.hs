module Main where

import Hw6
import Testing
import Hw6Tests


main :: IO()
main = do
    putStrLn "running tests..."
    let a = runTests allTests
    if null a
        then putStrLn "all tests ok"
        else print a
    print ("fib 24 = 75025", fib 24)
    print ("first 25 fibs = ", take 25 fibs1)
    putStrLn "done..."
    putStrLn appMsg
