module Main where

import           Party            (glPrintStr, maxFun)
import           System.Directory (doesFileExist)

main :: IO ()
main = do
    putStrLn "homework 7...\n"
    p <- doesFileExist "company.txt"
    if not p
        then putStrLn "company.txt not found."
        else do
            str <- readFile "company.txt"
            let gls = (maxFun . read) str
            putStrLn $ glPrintStr gls
    putStrLn "\ndone..."
