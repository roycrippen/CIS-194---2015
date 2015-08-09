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
        testGroup "sRepeat, sTake, streamToList, sInterleave" [
        testProperty "Create Int streams" prop_IntStreamCreate,
        testProperty "Create String streams" prop_StringStreamCreate,
        testProperty "Create String List streams" prop_StringListStreamCreate,
        testProperty "All elements the same" prop_AllElementsEqual,
        testProperty "Every other element equal" prop_Interleave
            ],
        testGroup "fmap stream" [
                testProperty "mapping Integer List" prop_IntegerFmap,
                testProperty "mapping String List" prop_StringFmap
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

-- Exercise 3 and 5 -----------------------------------------
prop_IntStreamCreate :: Int -> Int -> Bool
prop_IntStreamCreate n a =
    sTake n (sRepeat a) == replicate n a

prop_StringStreamCreate :: Int -> String -> Bool
prop_StringStreamCreate n a =
    sTake n (sRepeat a) == replicate n a

prop_StringListStreamCreate :: Int -> [String] -> Bool
prop_StringListStreamCreate n a =
    sTake n (sRepeat a) == replicate n a

prop_AllElementsEqual :: NonNegative Int -> NonNegative Int -> Maybe [String] -> Bool
prop_AllElementsEqual (NonNegative m) (NonNegative n) a =
    streamToList (sRepeat a) !! m == streamToList (sRepeat a) !! n

prop_Interleave :: NonNegative Int -> String -> String -> Bool
prop_Interleave (NonNegative n) s1 s2 =
    let s = sInterleave (sRepeat s1) (sRepeat s2)
        x  = streamToList s !! n
        x' = streamToList s !! (n+2)
        y  = streamToList s !! (n+1)
        y' = streamToList s !! (n+3)
    in x == x' && y == y'

-- Exercise 4 -----------------------------------------
prop_IntegerFmap :: Integer -> Bool
prop_IntegerFmap n =
    sTake 10 (fmap (*10) (sRepeat n)) == map (*10) (replicate 10 n)

prop_StringFmap :: String -> Bool
prop_StringFmap s =
    sTake 10 (fmap (\x -> x ++ x) (sRepeat s)) == map (\x -> x ++ x) (replicate 10 s)
