module Main where

import           Control.Monad.Random (evalRandIO)
import           Risk

main :: IO ()
main = do
    putStrLn "homework 12...\n"
    putStrLn "P(attacker wins) from 1000 invades of (attackers, defenders)"
    winPercent1 <- evalRandIO $ successProb (Battlefield 10 10)
    print ("P(attacker wins) (10, 10)     = ", winPercent1)
    winPercent2 <- evalRandIO $ successProb (Battlefield 50 50)
    print ("P(attacker wins) (50, 50)     = ", winPercent2)
    winPercent3 <- evalRandIO $ successProb (Battlefield 100 100)
    print ("P(attacker wins) (100, 100)   = ", winPercent3)
    winPercent4 <- evalRandIO $ successProb (Battlefield 500 500)
    print ("P(attacker wins) (500, 500)   = ", winPercent4)
    winPercent5 <- evalRandIO $ successProb (Battlefield 1000 1000)
    print ("P(attacker wins) (1000, 1000) = ", winPercent5)
    putStrLn "\n P(attacker wins 2 units) should be approximately 37.2%"
    pAttWins2 <- evalRandIO successWinPercentage
    print ("1,000,000 sample rolls P(attacker wins 2 units) = ", pAttWins2)
    putStrLn "\ndone..."
