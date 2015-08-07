module Main where

import Hw3
import Testing
import Hw3Tests


main :: IO()
main = do
    putStrLn "running tests..."

    -- unit tests
    let a = runTests allTests
    if null a
        then putStrLn "all unit tests ok\n"
        else print a

    -- three individual system test
    putStrLn "three system test showng values"
    let fact8 = state (run (extend empty "In" 8) factorial) "Out"  -- out = 40,320
    print ("8! = 40320", fact8)
    let sqrt65 = state (run (extend empty "A" 65) squareRoot) "B"  -- B = 8
    print ("sqrt 65 = 8", sqrt65)
    let fib25 = state (run (extend empty "In" 25) fibonacci) "Out"  -- Out = 75,025
    print ("fib 25 = 75025", fib25)

     -- 100 random systems tests of factorial 0 to factorial 12
    factList <- sample 100 [0..12]
    let factResultList = map factRun factList
    let factMsg = if False `notElem` factResultList
                     then "\n100 random tests of factorial 0 to 12 passed"
                     else "\nsomething failed on 100 tests of factorail 0 to 12"
    putStrLn factMsg

     -- 100 random systems tests of int sqrt 0 to sqrt 1000
    sqrtList <- sample 100 [0..1000]
    let sqrtResultList = map sqrtRun sqrtList
    let sqrtMsg = if False `notElem` sqrtResultList
                     then "100 random tests of int square root 0 to 1000 passed"
                     else "something failed on 100 tests of int square root 0 to 1000"
    putStrLn sqrtMsg

     -- 100 random systems tests of fib 0 to fib 30
    fibList <- sample 100 [0..30]
    let fibResultList = map fibRun fibList
    let fibMsg = if False `notElem` fibResultList
                     then "100 random tests of fib 0 to 30 passed"
                     else "something failed on 100 tests of fib 0 to 30"
    putStrLn fibMsg
    putStrLn "done..."
