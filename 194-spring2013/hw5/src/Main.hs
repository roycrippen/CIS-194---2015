module Main where

import           Calc

main :: IO ()
main = do
    putStrLn "homework 5..."
    putStrLn $ "\nevalStr \"((2+3)*10) + 50\" == 100 = " ++ show (evalStr "((2+3)*10)+50")
    putStrLn "\ntestExp = parseExp lit add mul \"(3 * -4) + 5\""
    putStrLn $ "testExp :: Maybe Integer = " ++ show testInteger
    putStrLn $ "testExp :: Maybe Bool    = " ++ show testBool
    putStrLn $ "testExp :: Maybe MinMax  = " ++ show testMM
    putStrLn $ "testExp :: Maybe Mod 7   = " ++ show testSat

    putStrLn "\ndone..."

s :: String
s = "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp s :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp s :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp s :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp s :: Maybe Mod7
