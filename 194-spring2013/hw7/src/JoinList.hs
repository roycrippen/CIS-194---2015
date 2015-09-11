module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty
  mappend = (+++)

-- exercise 1 ----------------------------------
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _)    = m
tag (Append m _ _ ) = m

-- exercise 2 ----------------------------------
-- excercise 2.1
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                 = Nothing
indexJ i _ | i < 0             = Nothing
indexJ i jl | i >= getIndex jl = Nothing
indexJ _ (Single _ x)          = Just x
indexJ i  (Append _ l r)
    | i < sizeLeft = indexJ i l
    | otherwise    = indexJ (i - sizeLeft) r
        where sizeLeft = getIndex l

-- helper and test functions
-- get the index of a join list
getIndex :: (Sized b, Monoid b) => JoinList b a -> Int
getIndex = getSize . size . tag

-- join lis t to string
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- safe get value at index from list function
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

-- build a joint list from a list
buildJointList :: [a] -> JoinList Size a
buildJointList = foldr (\x acc -> Single (Size 1) x +++ acc) Empty

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
    | i < sizeLeft = dropJ i l +++ r
    | otherwise    = dropJ (i - sizeLeft) r
        where sizeLeft = getIndex l

-- excercise 2.3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                = Empty
takeJ i _ | i <= 0           = Empty
takeJ i jl | i > getIndex jl = jl
takeJ _ jl@(Single _ _)      = jl
takeJ i  (Append _ l r)
    | i < sizeLeft = takeJ i l
    | otherwise    = l +++ takeJ (i - sizeLeft) r
        where sizeLeft = getIndex l

str :: [String]
str =    [ "Finally, make a main function to run the editor interface using "
         , "your join-list backend in place of the slow String backend (see "
         , "StringBufEditor.hs for an example of how to do this). You should "
         , "create an initial buffer of type JoinList (Score, Size) String and "
         , "pass it as an argument to runEditor editor." ]

testStr :: Bool
testStr = (jlToList . buildJointList) str == str
















--
