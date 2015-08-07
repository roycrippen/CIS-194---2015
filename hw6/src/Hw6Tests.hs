module Hw6Tests where

import Hw6
import Testing
import System.Random (randomRIO)
import Data.List
import Control.Applicative

-- Exercise 0 example ------------------------------------
testLength :: String -> Int
testLength = length

testTrue :: Int -> Bool
testTrue n = n == 1

ex0Tests :: [Test]
ex0Tests = [ testF1 "example test" testLength
             [ ("abc", 3)
             , ("aaaa", 4), ("", 0)
             , (appMsg, 23)
             ]
           , testF1 "another example" testTrue

             [ (1, True)
             , (0, False)
             , (10, False)
             ]
           ]


-- Exercise 1 -----------------------------------------
ex1Tests :: [Test]
ex1Tests = []

-- Exercise 2 -----------------------------------------
ex2Tests :: [Test]
ex2Tests = []

-- Exercise 3 -----------------------------------------
ex3Tests :: [Test]
ex3Tests = []

-- Exercise 4 -----------------------------------------
ex4Tests :: [Test]
ex4Tests = []

-- Exercise 5 -----------------------------------------
ex5Tests :: [Test]
ex5Tests = []

-- Exercise 6 -----------------------------------------
ex6Tests :: [Test]
ex6Tests = []


-- All Tests -----------------------------------------
allTests :: [Test]
allTests = concat [ ex0Tests
                  , ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]

-- system tests -------------------------------------------

-- random list generator of n elements
-- example in main: myList = sample n $ allPossibilitiesList
-- below, 100 element list of random ints between 0 and 1000
--                  myIntList = sample 100 $ [0..1000]

sample :: (Eq t) => Int -> [t] -> IO [t]
sample 0 _ = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> pure val <*> sample (l-1) (delete val xs)

sample1 :: [b] -> IO b
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx
