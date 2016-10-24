module Main where

import           Hw2

main :: IO()
main = do
    -- single detailed solution steps
    putStrLn "CIS 194, Spring 2015 assignment 2"
    putStrLn "\ndetailed moves of a 6 code test in reverse order"
    let sc = [Blue, Green, Red, Purple, Purple, Orange]
    putStrLn "code to solve:"
    putStrLn $ "     " ++ show sc
    _ <- mapM print $ solve sc
    putStrLn "done..."
