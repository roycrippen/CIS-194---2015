{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}

module Hw7 where

import           Cards
import           Data.List            (transpose)

import           Control.Monad        (liftM, liftM2, replicateM)
import           Control.Monad.Random (Rand, evalRandIO, getRandom, getRandomR)
import           Data.Functor
import           Data.Monoid
import           Data.Vector          (Vector, (!), (!?), (//))
import qualified Data.Vector          as V
import           Debug.Trace          (traceShow)
import           System.Random

traceShow' arg = traceShow arg arg

-- Exercise 1 -----------------------------------------
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f ma  = do   -- readable
    a <- ma
    return (f a)

liftM'' :: Monad m => (a -> b) -> m a -> m b
liftM'' f ma  = ma >>= \a -> return (f a)  -- consise

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV ix iy v = liftM2 f x y
    where x = v !? ix
          y = v !? iy
          f x' y' = v // [(ix, y'), (iy, x')]

-- Exercise 2 -----------------------------------------
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence (map f as)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts as vect = mapM (vect !?) as

-- Exercise 3 -----------------------------------------
type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vect
    | len == 0 = liftM (vect !?) (getRandomR (1, 1)) -- force a Rnd (Nothing)
    |otherwise = liftM (vect !?) (getRandomR (0, len - 1))
    where len = V.length vect

-- Exercise 4 -----------------------------------------
randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList (replicateM n getRandom)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (lo, hi) = liftM V.fromList (replicateM n $ getRandomR (lo, hi))

-- Exercise 5 -----------------------------------------
shuffle :: Vector a -> Rnd (Vector a)
shuffle = undefined

unsafeSwapV :: Int -> Int -> Vector a -> Vector a
unsafeSwapV ix iy v = f x y
    where x = v ! ix
          y = v ! iy
          f x' y' = v // [(ix, y'), (iy, x')]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt = undefined

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort = undefined

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else -- do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

--main :: IO ()
--main = evalRandIO newDeck >>= repl . State 100


-- Exercise 10 ----------------------------------------
newtype Matrix a = M [[a]] deriving (Eq, Show)

mFib :: Matrix Integer
mFib = M [[1,1], [1,0]]

instance Num a => Num (Matrix a) where
    M a * M b = M [ [ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]
    M a + M b = M (zipWith (zipWith (+)) a b)
    M a - M b = M (zipWith (zipWith (-)) a b)
    negate (M a) = M (map (map negate) a)
    fromInteger x = M (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1

fastFib :: Int -> Integer
fastFib n = head . head $ matrix
    where (M matrix) = mFib^n
