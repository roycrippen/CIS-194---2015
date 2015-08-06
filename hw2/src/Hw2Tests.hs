-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module Hw2Tests where

import Hw2
import Testing
import System.Random
import Data.List
import Control.Applicative

-- Exercise 1 -----------------------------------------
ex1Tests :: [Test]
ex1Tests = [ testF2 "exactMatches test" exactMatches
             [ ([Red, Blue, Green, Yellow], [Blue, Green, Yellow, Red], 0)
             , ([Red, Blue, Green, Yellow], [Red, Purple, Green, Orange], 2)
             , ([Green, Yellow, Purple, Purple, Orange, Blue], [Red, Yellow, Red, Purple, Red, Blue], 3)
             ]
           ]

-- Exercise 2 -----------------------------------------
ex2Tests :: [Test]
ex2Tests = [ testF1 "countColors test" countColors
             [ ([Red, Blue, Yellow, Purple], [1, 0, 1, 1, 0, 1])
             , ([Green, Blue, Green, Orange], [0, 2, 1, 0, 1, 0])
             , ([Red, Red, Red, Red, Red, Red], [6, 0, 0, 0, 0, 0])
             ]
           , testF2 "matches test" matches
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue], 3)
             , ([Green, Yellow, Purple, Purple, Orange, Blue], [Red, Yellow, Red, Purple, Orange, Blue], 4)
             ]
           ]

-- Exercise 3 -----------------------------------------
ex3Tests :: [Test]
ex3Tests = [ testF2 "getMove test" getMove
             [ ([Red, Blue, Yellow, Orange], [Red, Orange, Orange, Blue],
               Move [Red, Orange, Orange, Blue] 1 2)
             ]
           ]

-- Exercise 4 -----------------------------------------
ex4Tests :: [Test]
ex4Tests = [ testF2 "isConsistent test" isConsistent
             [ (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Yellow, Purple],
               True)
             , (Move [Red, Red, Blue, Green] 1 1, [Red, Blue, Red, Purple],
               False)
             ]
           ]

-- Exercise 6 -----------------------------------------
allCodesCount :: Int -> Int
allCodesCount n = length $ allCodes n

ex6Tests :: [Test]
ex6Tests = [ testF1 "allCodes count test" allCodesCount
             [(4, 1296), (6, 46656)]
           ]

-- Exercise 7 -----------------------------------------
solveCode :: Code -> Code
solveCode [] = []
solveCode c = getCode hd
    where (hd:_) = solve c

ex7Tests :: [Test]
ex7Tests = [ testF1 "solve" solveCode
            [([Purple, Red], [Purple, Red])
            ,([Purple, Orange, Yellow, Blue, Green, Red], [Purple, Orange, Yellow, Blue, Green, Red])
            ,([Purple, Purple, Purple, Purple, Purple, Purple], [Purple, Purple, Purple, Purple, Purple, Purple])
            ,([], [])
            ]
           ]

-- All Tests ------------------------------------------
allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex6Tests
                  , ex7Tests
                  ]


sample1 :: [b] -> IO b
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx

sample :: (Eq t) => Int -> [t] -> IO [t]
sample 0 _ = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> (pure val) <*> (sample (l-1) (delete val xs))


