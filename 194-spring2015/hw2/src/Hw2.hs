module Hw2 where

import           Control.Monad

data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

type Code = [Peg]

data Move = Move Code Int Int
          deriving (Show, Eq)

getCode :: Move -> Code
getCode (Move c _ _) = c

getEm :: Move -> Int
getEm (Move _ em _) = em

getNem :: Move -> Int
getNem (Move _ _ nem) = nem

colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------
-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches sc gc = sum . map (\(a, b) -> if a == b then 1 else 0) $ zip sc gc

-- Exercise 2 -----------------------------------------
-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map sumOfOneColor colors
    where sumOfOneColor :: Peg -> Int
          sumOfOneColor p = sum $ map (\c -> if c == p then 1 else 0) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches sc gc = sum $ map (\(a, b) -> if a <= b then a else b) zippedCodes
    where zippedCodes = zip (countColors sc) (countColors gc)

-- Exercise 3 -----------------------------------------
-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove sc gc = Move gc eMatch (matches sc gc - eMatch)
    where eMatch = exactMatches sc gc

-- Exercise 4 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent move gc = eMatch == getEm move && allMatch == getEm move + getNem move
    where eMatch = exactMatches (getCode move) gc
          allMatch = matches (getCode move) gc

-- Exercise 5 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes move c =
    filter (/= []) $ map (\x -> if isConsistent move x then x else []) c

-- Exercise 6 -----------------------------------------
allCodes :: Int -> [Code]
allCodes n = replicateM n colors

-- Exercise 7 -----------------------------------------
solve :: Code -> [Move]
solve [] = []
solve sc =
    solve' (allCodes (length sc)) []
    where solve' :: [Code] -> [Move] -> [Move]
          solve' [] _ = []
          solve' (c:cs) ml =
              case length l of
                  0 -> error "could not find answer"
                  1 | getEm move == length sc -> move : ml -- done, answer is in list
                  1 -> getMove sc (head l) : move : ml -- done, add answer to list
                  _ -> solve' l (move : ml)
              where move = getMove sc c
                    l = filterCodes move (c:cs)
