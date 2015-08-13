{-
to run tests -----------------------
   cabal configure --enable-tests
   cabal build
   cabal test      or   cabal test --show-details=always
   best is to run test executable from dist/build/<test dir>...
-}

import           Control.Monad.Random
import           Data.Vector as V
import           Hw7
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "group 1 hw7" [
                testProperty "fastFib n = 0 to 10,000" prop_FastFib,
                testCase "trivial" test_Aaa
            ]
        ]









-- Exercise 10 -----------------------------------------






test_Aaa :: Assertion
test_Aaa = True @?= True

nonNegativeIntegers :: Gen Int
nonNegativeIntegers = choose (0, 10000)

prop_FastFib :: Property
prop_FastFib =
    forAll nonNegativeIntegers $ \n ->
    let x = fastFib n
        y = fastFib (n+1)
        z = fastFib (n+2)
    in x + y == z
