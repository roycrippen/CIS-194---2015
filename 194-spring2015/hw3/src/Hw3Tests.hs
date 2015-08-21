-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module Hw3Tests where

import Hw3
import Testing
import qualified Data.Map as Map
import System.Random
import Data.List
import Control.Applicative


-- Exercise 1 -----------------------------------------
ex1Tests :: [Test]
ex1Tests = [ testF3 "extend: add element to state list" extend
             [ (Hw3.empty, "a", 1, Map.fromList [("a",1)])
             , (Map.fromList [("a",1)], "b", 2, Map.fromList [("a",1),("b",2)])
             , (Map.fromList [("a",1),("b",2)], "a", 10, Map.fromList [("a",10),("b",2)])
             , (Map.fromList [("a",1),("b",2)], "", 1, Map.fromList [("a",1),("b",2)])
             ]
             , testF2 "state: get a variable value" state
             [ (Hw3.empty, "a", 0)
             , (Map.fromList [("a",1)], "a", 1)
             , (Map.fromList [("a",1)], "b", 0)
             ]
           ]

-- Exercise 2 -----------------------------------------
ex2Tests :: [Test]
ex2Tests = [ testF2 "evalE: test execution of expression" evalE
             [ (Hw3.empty, Val 1, 1)
             , (Hw3.empty, Op (Val 1) Plus (Val 2), 3)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "a") Plus (Var "c"), 4)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "a") Plus (Var "none"), 1)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Minus (Var "c"), 0)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Times (Var "c"), 9)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Divide (Var "c"), 1)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Divide (Val 0), 0)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Gt  (Var "c"), 0)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Ge  (Var "c"), 1)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Lt  (Var "c"), 0)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Le  (Var "c"), 1)
             , (Map.fromList [("a",1),("b",2),("c",3)], Op (Var "c") Eql (Var "c"), 1)
             ]
           ]


-- All Tests -----------------------------------------
allTests :: [Test]
allTests = ex1Tests ++ ex2Tests

-- system tests ------------------------------
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
  (:) <$> pure val <*> sample (l-1) (delete val xs)

factRun :: Int -> Bool
factRun n = state (run (extend Hw3.empty "In" n) factorial) "Out" == fact n

fact :: Int -> Int
fact n = if n == 0 || n == 1 then 1 else n * fact (n - 1)

intSqrt :: Int -> Int
intSqrt = floor . (sqrt :: Double -> Double) . fromIntegral

sqrtRun :: Int -> Bool
sqrtRun n = state (run (extend Hw3.empty "A" n) squareRoot) "B" == intSqrt n

fibRun :: Int -> Bool
fibRun n = state (run (extend Hw3.empty "In" n) fibonacci) "Out" == fib n

fib :: Int -> Int
fib n =
    case n of
        0 -> 0
        1 -> 1
        _ -> fib(n - 1) + fib(n - 2)
