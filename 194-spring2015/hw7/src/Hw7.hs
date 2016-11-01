{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}

module Hw7 where

import           Cards
import           Control.Monad        (liftM2, replicateM)
import           Control.Monad.Random (Rand, getRandom, getRandomR)
import           Data.Monoid
import           Data.Vector          (Vector, (!), (!?), (//))
import qualified Data.Vector          as V
import           System.IO
import           System.Random

--- Exercise 1 -----------------------------------------
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
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]--rnd0to9 :: IO ()
mapM' f as = sequence (map f as)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts as vect = mapM (vect !?) as

-- Exercise 3 -----------------------------------------
type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vect
    | len == 0 = liftM' (vect !?) (getRandomR (1, 1)) -- force a Rnd (Nothing)
    |otherwise = liftM' (vect !?) (getRandomR (0, len - 1))
    where len = V.length vect

-- Exercise 4 -----------------------------------------
randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM' V.fromList (replicateM n getRandom)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n (lo, hi) = liftM' V.fromList (replicateM n $ getRandomR (lo, hi))

-- Exercise 5 -----------------------------------------
shuffle :: Vector a -> Rnd (Vector a)
shuffle vect = go vect (V.length vect)
    where go :: Vector a -> Int -> Rnd (Vector a)
          go v i
              | i == 1    = return v
              | otherwise = do
                 let i' = i - 1
                 rnd <- getRandomR (0, i')
                 go (unsafeSwapV i' rnd v) i'

unsafeSwapV :: Int -> Int -> Vector a -> Vector a
unsafeSwapV ix iy v = f x y
    where x = v ! ix
          y = v ! iy
          f x' y' = v // [(ix, y'), (iy, x')]

-- Exercise 6 -----------------------------------------
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vect pivotIdx = (lesserVect, pivotVal, greaterVect)
    where pivotVal = vect ! pivotIdx
          (l, r) = V.splitAt pivotIdx vect
          vect' = l V.++ V.drop 1 r
          lesserVect = V.filter (< pivotVal) vect'
          greaterVect = V.filter (>= pivotVal) vect'

-- Exercise 7 -----------------------------------------
-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
    <> (x : quicksort [ y | y <- xs, y >= x ])

-- readable quicksort
quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' lesser ++ [x] ++ quicksort' greater
    where lesser  = [ y | y <- xs, y < x ]
          greater = [ y | y <- xs, y >= x ]

qsort :: Ord a => Vector a -> Vector a
qsort vect
    | V.length vect == 0 = vect
    | otherwise          = qsort lesser V.++ x V.++ qsort greater
        where (x,xs) = V.splitAt 1 vect
              lesser  = [ y | y <- xs, y <  x ! 0 ]
              greater = [ y | y <- xs, y >=  x ! 0 ]

-- Exercise 8 -----------------------------------------
-- much quicker than qsort
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vect
    | V.length vect == 0 = return vect
    | otherwise          = do
        rnd <- getRandomR (0, V.length vect - 1)
        let (lesser, pivot, greater) = partitionAt vect rnd
        l <- qsortR lesser
        r <- qsortR greater
        return (l V.++ V.cons pivot  r)

-- Exercise 9 -----------------------------------------
-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank vect
    | V.length vect == 0 = return Nothing
    | otherwise          = do
        rnd <- getRandomR (0, V.length vect - 1)
        let (lesser, pivot, greater) = partitionAt vect rnd
            lenLeft = V.length lesser
        case () of
            _| rank < lenLeft  -> select rank lesser
             | rank == lenLeft -> return $ Just pivot
             | otherwise       -> select (rank - lenLeft - 1) greater

allCards :: Deck
allCards = [ Card l s | s <- suits, l <- labels ]

allCards' :: Deck
allCards' = do s <- suits
               l <- labels
               return (Card l s)

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------
nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
    | V.length d == 0 = Nothing
    | otherwise       = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------
getCards :: Int -> Deck -> Maybe ([Card], Deck)
--getCards = undefined
getCards n d
    | V.length d < n = Nothing
    | otherwise      = go n d []
        where go 0  d' cs  = Just (cs, d')
              go n' d' cs = do
                  (c, deck) <- nextCard d'
                  go (n' - 1) deck (c:cs)

-- Exercise 13 ----------------------------------------
data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStr "Would you like to play (y/n)? "
  hFlush stdout
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStr "How much do you want to bet? "
            hFlush stdout
            amt' <- readMaybe <$> getLine  --fixes read error
            let amt = maybe 0 (+0) amt' -- Nothing -> 0 else amount
            if amt < 1 || amt > money
            then play
            else -- do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:" ++ show c1
                  putStrLn $ "  I got:" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            hFlush stdout
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got:" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "  I got:" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

--main :: IO ()
--main = evalRandIO newDeck >>= repl . State 100

-- orginal version crashed on amt read for non-number
readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing
