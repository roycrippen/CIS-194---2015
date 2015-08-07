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
    putStrLn "done..."
    putStrLn appMsg
