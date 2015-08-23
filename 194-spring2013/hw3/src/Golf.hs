{-# OPTIONS_GHC -Wall #-}
module Golf where
import           Control.Monad (guard)
import Data.List (sort, group, transpose)

-- exercise 1 ---------------
skips :: [a] -> [[a]] -- could be shorter with list value xs renamed s
skips xs = map (\(i,_) -> [snd x | x <- z xs, fst x `mod` i == 0]) $ z xs

z :: [b] -> [(Int, b)]
z = zip [1..]  -- space saving redefine used twice

-- skips' is used to explain skip, do block version of list comprehension
skips' :: [t] -> [[t]]
skips' xs = map (\(i,_) ->
                do x <- zip ([1..]::[Int]) xs -- do block produces a list of every Ith element using zip index
                   guard (fst x `mod` i == 0) -- ensures only every Ith elemnets taken
                   return (snd x)) -- return Ith list without index
             (zip [1..] xs)  -- gives an index i for list comprehension to count across

-- skip' "ABCD"
        -- outer zip for map = [(1,'A'), (2, 'B'), (3, 'B'), (4,'D')], giving index i -> 1..4
        -- first time throughg return all letters, ie n `mod` i == 0 forall n where i = 1
        -- second time through map (\(i,_) -> do ...) i = 2
            -- x <- [(1,'A'), (2, 'B'), (3, 'B'), (4,'D')]
            -- guard -> forall a, fst a `mod` 2 == 0  gives  1==0, 0==0, 1==0, 0==0 or F,T,F,T
                -- therfore 2nd and 4th elements kept
            -- return (snd a) -> 'B' : 'D' = "BD"

-- exercise 2 ---------------
localMaxima :: [Integer] -> [Integer]
localMaxima (l:xs@(m:r:_))  -- l is left side, m is candidate, r is right side
  | m > l && m > r = m : localMaxima xs -- cons list if m is a local maximum
  | otherwise      = localMaxima xs   -- otherwise keep looking
localMaxima _ = []  -- will end when there are less than three elements left

--example [2,9,5,6,1]
--         l,m,r   xs = [9,5,6,1], local max so 9: localMaxima xs
--        [9,5,6,1]
--         l,m,r     not a local max so localMaxima [5,6,1]
--        [5,6,1] local max, so 6: localMaxima [6,1]
--        [6,1] -> done, rollback collecting 6 and 9 -> [9,6]

-- exercise 3 ---------------
histogram :: [Integer] -> String
histogram xs = concat build ++ "==========\n" ++ "0123456789\n"
  where
    build = map ((++ "\n") . f) . reverse . transpose . group $ sort xs

f :: [Integer] -> String
f ls = go n' ls [] where           -- map over n' looking for matches in the list
    n' = [0,1,2,3,4,5,6,7,8,9]     -- f [0,2,4,6,8] == "* * * * * "
    go ns xs ys = foldr (\n -> (:) (if n `elem` xs then '*' else ' ')) ys ns

-- histogram' with intermediate steps to explain histogram
histogram' :: [Integer] -> String
histogram' xs = answer
  where
    orderLst = reverse . transpose . group $ sort xs -- order group and transpose list
                                                     -- orderLst [1,4,5,4,6,6,3,4,2,4,9] =
                                                     -- [[4],[4],[4,6],[1,2,3,4,5,6,9]]
                                                     -- ready to build histogram string
    buildStr = map ((++ "\n") . f) orderLst  -- map across this list using helper function f that does the real work
    answer = concat buildStr ++ "==========\n" ++ "0123456789\n" -- add on the last parts to the string
