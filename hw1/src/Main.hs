module Main where

import           Hw1
import           Hw1Tests
import           Testing

l :: Integer
l = 5594589764218858

main :: IO()
main = do
    putStrLn "running tests..."
    let a = runTests allTests
    if null a
        then putStrLn "all tests ok"
        else print a
    putStrLn "\n"
    print ("luhn", l, luhn l)
    print ("hanoi 3", hanoi 3 "from" "to" "helper" )
