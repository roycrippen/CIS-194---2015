module Main where

import JoinList

main :: IO ()
main = do
    putStrLn "homework 7..."
    putStrLn "\nfor all ['a'..'z'], indexJ i == !!? i "
    print $ testIndex (buildJointList ['a'..'z']) ['a'..'z']
    putStrLn "\njlToList (dropJ 10 jl) == drop n (jlToList jl) "
    print $ testDrop 10 $ buildJointList ['a'..'z']
    putStrLn "\njlToList (takeJ n jl) == take n (jlToList jl) "
    print $ testTake 10 $ buildJointList ['a'..'z']
    putStrLn "\ndone..."
