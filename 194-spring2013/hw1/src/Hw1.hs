module Main where

main :: IO ()
main = do
    putStrLn "homework 1..."
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

-- exercise 1 ---------------
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- exercise 2 ---------------
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (dropLastDigit n) ++ [ lastDigit n ]

-- exercise 3 ---------------
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse . doubleEveryOther' $ reverse xs
    where doubleEveryOther' :: [Integer] -> [Integer]
          doubleEveryOther' []       = []
          doubleEveryOther' [x]      = [x]
          doubleEveryOther' (x:y:ys) = x : (y * 2) : doubleEveryOther' ys

-- exercise 4 ---------------
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs

-- exercise 5 ---------------
validate :: Integer -> Bool
validate n
    | n < 0 || n > 9999999999999999 = False
    | otherwise = lastDigit (sumDigits . doubleEveryOther $ toDigits n)  == 0

-- exercise 6 ---------------
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 2 a b c _ = [(a,c), (a,b), (c,b)]
hanoi4 n a b c d = hanoi4 (n-2) a c b d ++
                   [(a,d), (a,b), (d,b)] ++
                   hanoi4 (n-2) c b d a



--
