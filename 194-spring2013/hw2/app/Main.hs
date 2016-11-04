module Main where

import           Log         (testParse, testWhatWentWrong)
import           LogAnalysis (parse, testSolution, whatWentWrong)

main :: IO ()
main = do
    putStrLn "CIS 194, Spring 2013 assignment 2"
    putStrLn ""
    putStrLn "test parse smaple.log"
    testParsing <- testParse parse 100 "./data/sample.log"
    mapM_ print testParsing
    error1  <- testWhatWentWrong parse whatWentWrong "./data/error.log"
    error2 <- testSolution "./data/error.log"
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

