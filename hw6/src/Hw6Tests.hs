module Hw6Tests where

import Hw6
import Testing
--import Data.List
--import Control.Applicative
import Test.QuickCheck

-- Exercise 1 -----------------------------------------
tinyNonNegativeIntegers :: Gen Int
tinyNonNegativeIntegers = choose (0, 25)

prop_Fibonacci1 :: Property
prop_Fibonacci1 =
  forAll tinyNonNegativeIntegers $ \n ->
    let x = fibs1 !! (n)
        y = fibs1 !! (n+1)
        z = fibs1 !! (n+2)
    in x + y == z

-- Exercise 2 -----------------------------------------
smallNonNegativeIntegers :: Gen Int
smallNonNegativeIntegers = choose (0, 500)

prop_Fibonacci2 :: Property
prop_Fibonacci2 =
  forAll smallNonNegativeIntegers $ \n ->
    let x = fibs2 !! (n)
        y = fibs2 !! (n+1)
        z = fibs2 !! (n+2)
    in x + y == z

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
ex0Tests :: [Test]
ex0Tests = []
ex1Tests :: [Test]
ex1Tests = []
ex2Tests :: [Test]
ex2Tests = []

allTests :: [Test]
allTests = concat [ ex0Tests
                  , ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
