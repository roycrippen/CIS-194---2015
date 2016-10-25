module Main where

import           Hw4

main :: IO()
main = do
    putStrLn "CIS 194, Spring 2015 assignment 3"
    putStrLn ""
    let p1 = 3*x^3 + 2*x^2 + x + 10 :: Poly Int
        v1 = applyP p1 10
    putStrLn $ "for " ++ show p1 ++ ", apply x to 10 = " ++ show v1

    putStrLn "\nbig foil of (3*x^3 + 2*x^2 + x + 10)^3 = "
    print $ p1^3

    putStrLn "\nderivitives"
    putStrLn $ "d/dx " ++ show p1 ++ " = " ++ show (deriv p1)
    let p2 = P[1.1,2.2,3.3] :: Poly Double
    putStrLn $ "d/dx " ++ show p2 ++ " = " ++ show (deriv p2)
    let p3 = x^2 + 3*x + 5 :: Poly Int
    putStrLn $ "d/dx " ++ show p3 ++ " = " ++ show (deriv p3)

    putStrLn "\nnth derivitive n = 1 -> 6"
    print (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5)
    print (nderiv 1 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 2 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 3 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 4 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 5 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 6 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))

    putStrLn "\ndone..."
