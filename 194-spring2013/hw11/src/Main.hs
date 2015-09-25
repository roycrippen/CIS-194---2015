module Main where

import           AParser
import           SExpr

main :: IO ()
main = do
    putStrLn "homework 11...\n"
    putStrLn "\nparsing long string [1..10000] -> \"n abcn\""
    putStrLn $ "..." ++ drop (length str' - 100) str'
    putStrLn getExpr
    putStrLn "\ndone..."

str :: String
str = foldr (\x s -> ("(" ++ show x ++ " abc" ++ show x ++ ") " ++ s)) "" ([1..10000] :: [Int])

str' :: String
str' = "(" ++ str ++ ")"

getExpr :: String
getExpr = case runParser parseSExpr str' of
    Nothing -> "Error parsing"
    Just (v,_) -> "\n\n..." ++ drop (length s - 99) s
        where s = show v
