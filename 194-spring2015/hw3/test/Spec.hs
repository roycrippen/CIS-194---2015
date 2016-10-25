import           Control.Applicative
import           Data.List
import qualified Data.Map            as Map
import           Hw3
import           System.Random
import           Testing

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



main :: IO()
main = do
    putStrLn "\nrunning tests..."
    print allTests
    let a = runTests allTests
    if null a
        then putStrLn "all unit tests ok\n"
        else print a

     -- 100 random systems tests of factorial 0 to factorial 12
    factList <- sample 100 [0..12]
    let factResultList = map factRun factList
    let factMsg = if False `notElem` factResultList
                     then "\n100 random tests of factorial 0 to 12 passed"
                     else "\nsomething failed on 100 tests of factorail 0 to 12"
    putStrLn factMsg

     -- 100 random systems tests of int sqrt 0 to sqrt 1000
    sqrtList <- sample 100 [0..1000]
    let sqrtResultList = map sqrtRun sqrtList
    let sqrtMsg = if False `notElem` sqrtResultList
                     then "100 random tests of int square root 0 to 1000 passed"
                     else "something failed on 100 tests of int square root 0 to 1000"
    putStrLn sqrtMsg

     -- 100 random systems tests of fib 0 to fib 30
    fibList <- sample 100 [0..30]
    let fibResultList = map fibRun fibList
    let fibMsg = if False `notElem` fibResultList
                     then "100 random tests of fib 0 to 30 passed"
                     else "something failed on 100 tests of fib 0 to 30"
    putStrLn fibMsg
    putStrLn "done..."
