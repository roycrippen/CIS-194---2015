module Main where

import Data.List ((\\))

main :: IO ()
main = do
    putStrLn "homework 4..."
    putStrLn $ "\nfun1 [3..10000] == fun1' [3..10000] = " ++
            show (fun1 [3..10000] == fun1' [3..10000])
    putStrLn $ "\nmap fun2 [1..100] == map fun2' [1..100] = " ++
            show (map fun2 [1..100] == map fun2' [1..100])
    let xs = [1..2047] :: [Integer]
    let (Node h1 _ _ _ ) = foldTree xs
    let (Node h2 _ _ _ ) = foldTree $ xs ++ [2048]
    putStrLn "\nheight of a perfectly balanced tree [1..2047] == 10"
    putStrLn $ "height foldTree [1..2047] == 10 " ++ show (h1 == 10)
    putStrLn $ "height foldTree [1..2048] == 11 " ++ show (h2 == 11)
    putStrLn $ "\nmap' (\\x -> 25 * x - 100) [1..2047] == map (\\x -> 25 * x - 100) [1..2047] = " ++
                show (map' (\x -> 25 * x - 100 :: Integer) xs == map (\x -> 25 * x - 100) xs)
    putStrLn $ "\nsieveSundaram 15 == [3,5,7,11,13,17,19,23,29,31] = " ++
                show (sieveSundaram 15 == [3,5,7,11,13,17,19,23,29,31])
    putStrLn $ "\ncount of sieveSundaram 1000 = " ++
                show (length (sieveSundaram 1000) == 302)
    putStrLn "\ndone..."

-- exercise 1 -----------------------------
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun1'' :: [Integer] -> Integer
fun1'' = foldr (\x acc -> acc * (x-2)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/=1)
      . iterate (\x -> if even x then x `div` 2 else 3*x + 1)

-- exercise 2 -----------------------------
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where
        height :: Tree a -> Integer
        height Leaf = -1
        height (Node h _ _ _)  = h

        insert :: a -> Tree a -> Tree a
        insert v Leaf = Node 0 Leaf v Leaf
        insert v (Node h l x r)
            | lh < rh   = Node h (insert v l) x r
            | lh > rh   = Node h l x (insert v r)
            | otherwise = Node h' l' x r'
            where
                lh = height l
                rh = height r
                lTemp = insert v l
                rTemp = insert v r
                (h', l', r') = get lTemp rTemp
                 where get lt rt =
                            if height lt <= height rt
                            then (height lt + 1, lt, r)
                            else (height rt + 1, l, rt)

-- exercise 3 -----------------------------
xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> if x then acc + 1 else acc) (0 :: Int)

xor' :: [Bool] -> Bool
xor' = foldr1 (/=)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

-- exercise 4 -----------------------------
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ nats \\ exclude
  where nats = [1..n]
        tups = [(i,j) | i <- nats, j <- nats, i <= j]
        tups' = filter (\(i, j) -> i + j + 2*i*j <= n) tups
        exclude = map (\(i, j) -> i + j + 2*i*j) tups'
