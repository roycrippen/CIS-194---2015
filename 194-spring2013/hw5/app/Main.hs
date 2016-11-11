module Main where

import           Calc
import           StackVM

main :: IO ()
main = do
    putStrLn "CIS 194, Spring 2013 assignment 5"
    putStrLn ""
    putStrLn $ "\nevalStr \"((2+3)*10) + 50\" == 100 = " ++ show (evalStr "((2+3)*10)+50")
    putStrLn "\ntestExp = parseExp lit add mul \"(3 * -4) + 5\""
    putStrLn $ "testExp :: Maybe Integer = " ++ show testInteger
    putStrLn $ "testExp :: Maybe Bool    = " ++ show testBool
    putStrLn $ "testExp :: Maybe MinMax  = " ++ show testMM
    putStrLn $ "testExp :: Maybe Mod 7   = " ++ show testSat
    putStrLn $ "\ncompile \"(3+5)*10\" = " ++ show (compile "(3+5)*10")
    putStrLn $ "stackVM [PushI 3,PushI 5,Add,PushI 10,Mul] = " ++
                show (stackVM [PushI 3, PushI 5, Add, PushI 10, Mul])
    putStrLn $ "run \"(1+9)*(7+3)*10\" = " ++ show (run "(1+9)*(7+3)*10")
    putStrLn $ "\n" ++ wvStr ++ " = " ++ show wv
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

wv :: Maybe Integer
wv = withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))

wvStr :: String
wvStr = "withVars [(\"x\", 6), (\"y\", 3)] $ mul (var \"x\") (add (var \"y\") (var \"x\"))"
