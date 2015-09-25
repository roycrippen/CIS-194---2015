module Main where

import           AParser

main :: IO ()
main = do
    putStrLn "homework 10...\n"
    putStrLn $ "runParser posInt \"10bc\" -> " ++ show (runParser posInt "10bc")
    putStrLn $ "runParser posInt \"abc\" -> " ++ show (runParser posInt "abc")
    putStrLn $ "\nfirst (++ \"b\") (\"a\", \"z\")" ++ show (first (++ "b") ("a", "z"))
    putStrLn $ "\nrunParser abParser \"abcde\" -> " ++ show (runParser abParser "abcde")
    putStrLn $ "runParser abParser \"xycde\" -> " ++ show (runParser abParser "xycde")
    putStrLn $ "runParser abParser_ \"abcde\" -> " ++ show (runParser abParser_ "abcde")
    putStrLn $ "runParser abParser_ \"xycde\" -> " ++ show (runParser abParser_ "xycde")
    putStrLn $ "\nrunParser intPair \"10 20abc\" -> " ++ show (runParser intPair "10 20abc")
    putStrLn $ "runParser intPair \"10 abc\" -> " ++ show (runParser intPair "10 abc")
    putStrLn $ "\nrunParser intOrUppercase \"10abc\" -> " ++ show (runParser intOrUppercase "10abc")
    putStrLn $ "runParser intOrUppercase \"Abc\" -> " ++ show (runParser intOrUppercase "Abc")
    putStrLn $ "runParser intOrUppercase \"abc\" -> " ++ show (runParser intOrUppercase "abc")
    putStrLn "\ndone..."
