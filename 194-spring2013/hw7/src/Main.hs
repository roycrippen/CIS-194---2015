module Main where

import           JoinList

main :: IO ()
main = do
    putStrLn "homework 7..."
    putStrLn "\ncarol.txt -> JointList -> String == carol.txt"
    str <- readFile "carol.txt"
    print $ testStr str
    runEditor'
    putStrLn "\ndone..."

testStr :: String -> Bool
testStr s = (unlines . jlToList . buildScoreSizeJL . lines) s == s
