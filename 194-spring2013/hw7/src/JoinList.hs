{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import           Buffer
import           Data.Monoid
import           Editor
import           Scrabble
import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty
    mappend = (+++)

-- exercise 1 ----------------------------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l `mappend` tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m
tag (Append m _ _ ) = m

-- exercise 2 ----------------------------------
-- excercise 2.1
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                 = Nothing
indexJ i _ | i < 0             = Nothing
indexJ i jl | i >= getIdx jl   = Nothing
indexJ _ (Single _ x)          = Just x
indexJ i  (Append _ l r)
    | i < getIdx l = indexJ i l
    | otherwise    = indexJ (i - getIdx l) r

-- helper and test functions
-- get the index of a join list
getIdx :: (Sized b, Monoid b) => JoinList b a -> Int
getIdx = getSize . size . tag

-- join lis t to string
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- safe get value at index from list function
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

-- build a somewhat balanced join list from a list
buildSizeJL :: [a] -> JoinList Size a
--buildSizeJL = foldr (\x acc -> Single (Size 1) x +++ acc) Empty  -- unbalanced but works
buildSizeJL xs = foldl (\acc x -> acc +++ Single (Size 1) x) Empty ls +++
                 foldr (\x acc -> Single (Size 1) x +++ acc) Empty rs
                        where ls = take (length xs `div` 2) xs
                              rs = drop (length ls) xs

-- test indexJ with !!?
testIndex :: (Sized b, Monoid b, Eq a) => JoinList b a -> [a] -> Bool
testIndex jl l = all f [0..(length l + 1)]
    where f idx = indexJ idx jl == l !!? idx

-- test dropJ with drop
testDrop :: Eq a => Int -> JoinList Size a -> Bool
testDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

-- test takeJ with take
testTake :: Eq a => Int -> JoinList Size a -> Bool
testTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)

-- excercise 2.2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ i jl | i <= 0  = jl
dropJ _ (Single _ _) = Empty
dropJ i  (Append _ l r)
    | i < getIdx l = dropJ i l +++ r
    | otherwise    = dropJ (i - getIdx l) r

-- excercise 2.3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                = Empty
takeJ i _ | i <= 0           = Empty
takeJ i jl | i > getIdx jl   = jl
takeJ _ jl@(Single _ _)      = jl
takeJ i  (Append _ l r)
    | i < getIdx l = takeJ i l
    | otherwise    = l +++ takeJ (i - getIdx l) r

-- exercise 3 ----------------------------------
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- used for testing
buildScoreJL :: [String] -> JoinList Score String
buildScoreJL xs = foldl (\acc x -> acc +++ scoreLine x) Empty ls +++
                  foldr (\x acc -> scoreLine x +++ acc) Empty rs
                      where ls = take (length xs `div` 2) xs
                            rs = drop (length ls) xs

-- exercise 4 ----------------------------------
instance Buffer (JoinList (Score, Size) String) where
    toString           = unlines . jlToList
    fromString         = buildScoreSizeJL . lines
    line               = indexJ
    replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
    numLines           = getSize  . snd . tag
    value              = getScore . fst . tag

buildScoreSizeJL :: [String] -> JoinList (Score, Size) String
buildScoreSizeJL xs = foldl (\acc x -> acc +++ scoreLineSize x) Empty ls +++
                      foldr (\x acc -> scoreLineSize x +++ acc) Empty rs
                            where ls = take (length xs `div` 2) xs
                                  rs = drop (length ls) xs

scoreLineSize :: String -> JoinList (Score, Size) String
scoreLineSize s = Single (scoreString s, Size 1) s

buf :: JoinList (Score, Size) String
buf = (fromString . unlines)
       [ "This buffer is for notes you don't want to save, and for"
       , "evaluation of steam valve coefficients."
       , "To load a different file, type the character L followed"
       , "by the name of the file."
       ]

runEditor' :: IO ()
runEditor' = runEditor editor buf
