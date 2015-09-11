module Main where

import           JoinList
import           System.Directory (doesFileExist)

main :: IO ()
main = do
    putStrLn "homework 7..."
    p <- doesFileExist "carol.txt"
    if p
        then do
            putStrLn "\ncarol.txt -> JointList -> String == carol.txt"
            str <- readFile "carol.txt"
            print $ testStr str
        else putStrLn "carol.txt not found, test string -> jl -> string not run"
    runEditor'
    putStrLn "\ndone..."

testStr :: String -> Bool
testStr s = (unlines . jlToList . buildScoreSizeJL . lines) s == s
