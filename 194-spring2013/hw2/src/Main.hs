module Main where

import           Log         (testWhatWentWrong, testParse)
import           LogAnalysis (parse, testSolution, whatWentWrong)

main :: IO ()
main = do
    putStrLn "homework 2..."
    testParsing <- testParse parse 100 "error.log"
    mapM_ print testParsing
    error1  <- testWhatWentWrong parse whatWentWrong "error.log"
    error2 <- testSolution "error.log"
    putStrLn "\ntake 3 from testWhatWentWrong on error.log:"
    putStrLn "["
    mapM_ print (take 3 error1)
    putStrLn "...]\n\ntake 3 from testSolution on error.log:"
    putStrLn "["
    mapM_ print (take 3 error2)
    putStrLn "...]\n"
    putStrLn $ "total results of testWhatWentWrong == testSolution: "
               ++ show (error1 == error2)
    putStrLn "\ndone..."
