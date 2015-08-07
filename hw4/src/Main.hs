module Main where

import Hw4
import Testing
import Hw4Tests

main :: IO()
main = do
    putStrLn "\nrunning tests..."
    let a = runTests allTests
    if null a then putStrLn "all unit tests ok\n" else print a


    let p1 = 3*x^3 + 2*x^2 + x + 10
    let v1 = applyP p1 10
    print (p1, "apply 10 to x ->", v1)

    putStrLn ("\nbig foil")
    print ("(3*x^3 + 2*x^2 + x + 10)^3 = ", p1^3)

    putStrLn ("\nderivitives")
    print (p1, deriv p1)
    let p2 = P[1.1,2.2,3.3]
    print (p2, deriv p2)
    let p3 = x^2 + 3*x + 5:: [(Poly Int, Poly Int, Bool)]
    print (p3, deriv p3)

    putStrLn "\nnth derivitive n = 1 -> 6"
    print((5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 1 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 2 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 3 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 4 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 5 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))
    print (nderiv 6 (5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5))

    putStrLn "\ndone..."
