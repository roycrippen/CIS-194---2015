module Main where

import           Hw6

main :: IO()
main = do
    putStrLn "Homework 6 main..."
    putStrLn $ "fib 24 = " ++ show (fib 24)
    putStrLn $ "first 25 fibs1 = " ++ show (take 25 fibs1)
    putStrLn $ "first 25 fibs2 = " ++ show (take 25 fibs2)
    putStrLn $ "stream [1,2,3] = " ++  show (sRepeat [1,2,3] :: Stream [Integer])
    putStrLn $ "iterate x and o = " ++ show (sIterate ('x' :) "o")
    putStrLn $ "interleave a and b =  " ++ show (sInterleave (sRepeat 'a') (sRepeat 'b'))
    putStrLn $ "nats: sIterate (+1) 0 = " ++ show (sIterate (+1) 0 :: Stream Integer)
    putStrLn $ "ruler = " ++ show (take 32 $ streamToList ruler)
    putStrLn $ "random from seed 7666532 = " ++ show (rand 7666532)
    putStrLn $ "(min, max) of 1M randoms = " ++ show (minMax $ sTake 1000000 $ rand 7666532)
    putStrLn "done..."
