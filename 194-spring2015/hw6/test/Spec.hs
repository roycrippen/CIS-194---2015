-- to run tests: stack test

import           Hw6
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck

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
            ],
        testGroup "test ruler element 2^n - 1" [
        testCase "ruler element n=3" test_RulerElement3,
        testCase "ruler element n=6" test_RulerElement6,
        testCase "ruler element n=12" test_RulerElement12,
        testCase "ruler element n=20" test_RulerElement20
            ],
        testGroup "fastFib" [
                testProperty "fastFib n = 0 to 10,000" prop_FastFib
            ]
        ]

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
    sTake 10 (fmap (*10) (sRepeat n)) == replicate 10 ((* 10) n)

prop_StringFmap :: String -> Bool
prop_StringFmap s =
    sTake 10 (fmap (\x -> x ++ x) (sRepeat s)) == replicate 10 ((\ x -> x ++ x) s)

-- Exercise 6 -----------------------------------------
test_RulerElement3 :: Assertion
test_RulerElement3 = (streamToList ruler !! 7) @?= 3

test_RulerElement6 :: Assertion
test_RulerElement6 = (streamToList ruler !! 63) @?= 6

test_RulerElement12 :: Assertion
test_RulerElement12 = (streamToList ruler !! 4095) @?= 12

test_RulerElement20 :: Assertion
test_RulerElement20 = (streamToList ruler !! 1048575) @?= 20

-- Exercise 10 -----------------------------------------
nonNegativeIntegers :: Gen Int
nonNegativeIntegers = choose (0, 10000)

prop_FastFib :: Property
prop_FastFib =
    forAll nonNegativeIntegers $ \n ->
    let x = fastFib n
        y = fastFib (n+1)
        z = fastFib (n+2)
    in x + y == z
