{-
to run tests -----------------------
   cabal configure --enable-tests
   cabal build
   cabal test      or   cabal test --show-details=always
   best is to run test executable from dist/build/<test dir>...
-}

import Hw6

--import Data.Monoid (mempty)
--import Test.Framework.Options (TestOptions, TestOptions'(..))
--import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Fibonacci tests" [
                testProperty "fibs1" prop_Fibonacci1,
                testProperty "fibs2" prop_Fibonacci2
            ],
        testGroup "Streams" [
        testProperty "Create Int streams" prop_IntStreamCreate,
        testProperty "Create String streams" prop_StringStreamCreate,
        testProperty "Create String List streams" prop_StringListStreamCreate
            ]
    ]

simpleTest :: Assertion
simpleTest = True @?= True

-- Exercise 1 -----------------------------------------
tinyNonNegativeIntegers :: Gen Int
tinyNonNegativeIntegers = choose (0, 25)

prop_Fibonacci1 :: Property
prop_Fibonacci1 =
  forAll tinyNonNegativeIntegers $ \n ->
    let x = fibs1 !! n
        y = fibs1 !! (n+1)
        z = fibs1 !! (n+2)
    in x + y == z

-- Exercise 2 -----------------------------------------
smallNonNegativeIntegers :: Gen Int
smallNonNegativeIntegers = choose (0, 500)

prop_Fibonacci2 :: Property
prop_Fibonacci2 =
  forAll smallNonNegativeIntegers $ \n ->
    let x = fibs2 !! n
        y = fibs2 !! (n+1)
        z = fibs2 !! (n+2)
    in x + y == z

-- Exercise 3 -----------------------------------------
prop_IntStreamCreate :: Int -> Int -> Bool
prop_IntStreamCreate n a =
    takeStream n (stream a) == replicate n a

prop_StringStreamCreate :: Int -> String -> Bool
prop_StringStreamCreate n a =
    takeStream n (stream a) == replicate n a

prop_StringListStreamCreate :: Int -> [String] -> Bool
prop_StringListStreamCreate n a =
    takeStream n (stream a) == replicate n a










-- Exercise 2 -----------------------------------------
-- Exercise 2 -----------------------------------------
