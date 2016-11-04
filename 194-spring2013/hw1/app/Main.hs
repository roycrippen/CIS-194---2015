module Main where

import           Hw1

main :: IO ()
main = do
    putStrLn "CIS 194, Spring 2013 assignment 1"
    putStrLn ""
    putStrLn $ "last digit 1234 = " ++ show (lastDigit 1234)
    putStrLn $ "drop last digit 1234 = " ++ show (dropLastDigit 1234)
    putStrLn $ "to digits 1234 = " ++ show (toDigits 1234)
    putStrLn $ "doubleEveryOther [8,7,6,5] = " ++ show (doubleEveryOther [8,7,6,5])
    putStrLn $ "sum of digits [16,7,12,5] = " ++ show (sumDigits $ doubleEveryOther [8,7,6,5])
    putStrLn $ "validate 4012888888881881 True = " ++ show (validate 4012888888881881)
    putStrLn $ "validate 4012888888881882 False = " ++ show (validate 4012888888881882)
    putStrLn $ "hanoi 3 = " ++ show (hanoi 3 "from" "to" "temp")
    putStrLn $ "hanoi4 4 = " ++ show (hanoi4 4 "from" "to" "temp1" "temp2")
    putStrLn $ "32,767 moves for hanoi 3 peg 15 = " ++ show (length (hanoi 15 "from" "to" "temp") == 32767)
    putStrLn $ "509 moves for hanoi 4 peg 15 = " ++ show (length (hanoi4 15 "from" "to" "temp1" "temp2") == 509)
