module Hw1 where

-- Exercise 1 -----------------------------------------
-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------
-- each digit from an int becomes a list item
toRevDigits :: Integer -> [Integer]
toRevDigits n
    | n <= 0 = []
    | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------
-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther [x] = [x]
doubleEveryOther (x1:x2:xs) = x1 : x2 * 2 :  doubleEveryOther xs

-- Exercise 4 -----------------------------------------
-- flatten multi-digit ints to single digit
toRevDigitsList :: [Integer] -> [Integer]
--toRevDigitsList [] = []
--toRevDigitsList (x:xs) = toRevDigits x ++ toRevDigitsList xs
toRevDigitsList = foldr ((++) . toRevDigits) []

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []  = 0
sumDigits lst = sum $ toRevDigitsList lst

-- Exercise 5 -----------------------------------------
-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n
    | n < 1000000000000000 || n > 9999999999999999 = False  -- must be 16 digits
    | lastDigit (sumDigits $ doubleEveryOther $ toRevDigits n) == 0 = True
    | otherwise = False

-- Exercise 6 -----------------------------------------
-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
