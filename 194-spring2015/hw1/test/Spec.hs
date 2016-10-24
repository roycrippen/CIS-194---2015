import           Hw1
import           Testing

-- Exercise 1 -----------------------------------------
ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit" (\(n,d) -> lastDigit n == d)
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit" (\(n, d) -> dropLastDigit n == d)
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------
ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigits" (\(n, xs) -> toRevDigits n == xs)
             [(123, [3,2,1]), (0,[]), (456, [6,5,4])]
           ]

-- Exercise 3 -----------------------------------------
ex3Tests :: [Test]
ex3Tests = [ Test "doubleEveryOther" (\(xs, ys) -> doubleEveryOther xs == ys)
             [([3,2,1], [3,4,1]), ([],[]), ([6,5,4], [6,10,4])]
           ]

-- Exercise 4 -----------------------------------------
ex4Tests :: [Test]
ex4Tests = [ Test "toRevDigitsList" (\(xs, ys) -> toRevDigitsList xs == ys)
             [([3,4,1], [3,4,1]), ([],[]), ([6,10,4], [6,0,1,4])]
            , Test "sumDigits" (\(xs, d) -> sumDigits xs == d)
             [([3,4,1], 8), ([], 0), ([6,10,4], 11)]
          ]

-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d


ex5Tests :: [Test]
ex5Tests = [ Test "luhn" testLuhn
             [(5594589764218858, True)
             , (1234567898765432, False)
             , (55945897642188585594589764218858, False)
             , (12121414, False)]
           ]

-- Exercise 6 -----------------------------------------
testHanoi :: Integer -> Bool
testHanoi n = length (hanoi n "from" "to" "helper") == 2^n - 1

ex6Tests :: [Test]
ex6Tests = [ Test "hanoi" testHanoi
             [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
           ]


-- All Tests -----------------------------------------
allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
main :: IO ()
main = do
    putStrLn "\nrunning tests..."
    print allTests
    let a = runTests allTests
    if null a
        then putStrLn "all unit tests ok"
        else print a

