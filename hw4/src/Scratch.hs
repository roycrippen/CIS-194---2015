module Aaa where

l1 = [1.1,2.1,3.1]

l2 = [3.0,4.00,5.0,6.0]


padZip :: [a] -> [a] -> a -> [(a, a)]
padZip xs1 xs2 v =
    case length xs1 - length xs2 of
        n | n < 0 -> zip (padList xs1 v (negate n)) xs2
        n | n > 0 -> zip xs1 (padList xs2 v n)
        _         -> zip xs1 xs2
    where padList :: [a] -> a -> Int -> [a]
          padList l val len = l ++ replicate len val



l3 = padZip l1 l2 0
l4 = padZip l2 l1 0















main :: IO()
main = putStrLn "done"
