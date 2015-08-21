module Main where

import           Hw2
import           Hw2Tests
import           Testing

main :: IO()
main = do
    putStrLn "running tests..."
    -- unit tests
    let a = runTests allTests
    if null a
        then putStrLn "all unit tests ok\n"
        else print a

    -- single detailed steps test
    let sc = [Blue, Green, Red, Purple, Purple, Orange]
    putStrLn "detailed moves of a 6 code test"
    print ("sc", sc)
    _ <- mapM print $ solve sc

    -- 10 tests of random 6 color codes
    cList <-  sample 10 $ allCodes 6
    let resultList = map (\x -> x == getCode  (head $ solve x)) cList
    -- print resultList
    let resultMsg = if False `notElem` resultList
                        then "\n10 tests of 6 random codes passed"
                        else "\nsomething failed on 10 tests of 6 random codes"
    putStrLn resultMsg
    putStrLn "done..."
