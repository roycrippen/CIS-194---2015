module Hw1 where

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


