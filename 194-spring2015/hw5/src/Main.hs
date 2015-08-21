module Main where

import           Hw5
import           System.Environment (getArgs)

main :: IO()
main = do
    let pathStr = "./data/"
        cnt :: Int
        cnt = 10   -- default number of time to run, change with last arg
    args <- getArgs
    crim <-
        case args of
            -- shell command if executable is with data
            -- time ./hw5 dog-original.jpg dog.jpg transactions.json victims.json new-ids.json new-transactions.json 10
            dog1:dog2:trans:vict:ids:out:nStr:_ -> do
                let n = read nStr :: Int
                putStrLn $ "running " ++ show n ++ " time(s)..."
                mapM (\_ -> doEverything dog1 dog2 trans vict ids out) [1..n]
            _ -> do
                putStrLn $ "running " ++ show cnt ++ " time(s)..."
                mapM (\_ -> doEverything (pathStr ++ "dog-original.jpg")
                                         (pathStr ++ "dog.jpg")
                                         (pathStr ++ "transactions.json")
                                         (pathStr ++ "victims.json")
                                         (pathStr ++ "new-ids.json")
                                         (pathStr ++ "new-transactions.json")) [1..cnt]
    if null crim
        then putStrLn "invalid run count argument"
        else putStrLn $ "Criminal is " ++ head crim
