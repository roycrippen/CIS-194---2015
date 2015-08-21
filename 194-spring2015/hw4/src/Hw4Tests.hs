module Hw4Tests where

import           Hw4
import           Testing

-- Exercise 1 -----------------------------------------
ex1Tests :: [Test]
ex1Tests = [ testF2 "f(x) = x" testEq
             [ (x, P [0, 1] , True)
             , (x, P [2]    , False)
             , (x, P [2, 1] , False)
             ]
           ]

-- Exercise 2 -----------------------------------------
testEq :: (Num a, Eq a) => Poly a -> Poly a -> Bool
testEq a b  = a == b

testNe :: (Num a, Eq a) => Poly a -> Poly a -> Bool
testNe a b  = a /= b

ex2Tests :: [Test]
ex2Tests = [ testF2 "Eq test int" testEq
             [ (P [1,2,3]   , P [1,2,3], True)
             , (P [1,2]     , P [1,2,0], True)
             , (P [1,2,0,0] , P [1,2]  , True)
             , (P []        , P []     , True)
             , (P [1,2]     , P [1,2,4], False)
             , (P [1,2,3]   , P [1,2,4], False)
             , (P [1,2,0,0] , P []     , False)
             ]
           , testF2 "Ne (/=) test int" testNe
             [ (P [1,2,3]  , P [1,2,3], False)
             , (P [1,2]    , P [1,2,0], False)
             , (P [1,2,0,0], P [1,2]  , False)
             , (P []       , P []     , False)
             , (P [1,2]    , P [1,2,4], True)
             , (P [1,2,3]  , P [1,2,4], True)
             , (P [1,2,0,0], P []     , True)
             ]
           , testF2 "Eq test double" testEq
                [ (P [1.0,2.0,3.0] , P [1.0,2.0,3.0], True)
                , (P [1.0,2.0]     , P [1.0,2.0,0.0], True)
                , (P []            , P []     , True)
                , (P [1.0]         , P []     , False)
                ]
           , testF2 "Ne (/=) test" testNe
                [ (P [1.0,2.0,3.0] , P [1.0,2.0,3.0], False)
                , (P [1.0,2.0]     , P [1.0,2.0,0.0], False)
                , (P [1.1]         , P []     , True)
                ]
           ]

-- Exercise 3 -----------------------------------------
ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
             [ (P [1, 0, 0, 2]       , "2x^3 + 1"        )
             , (P [0, -1, 2]         , "2x^2 + -x"       )
             , (P [-4, 0, 0, 0]      , "-4"              )
             , (P [-1, -1, -2]       , "-2x^2 + -x + -1" )
             , (P [-1, 2, 2, 0]      , "2x^2 + 2x + -1"  )
             , (P [0, -2, 2]         , "2x^2 + -2x"      )
             , (P [0, 0, 0, 0]       , "0"               )
             , (P [0, 0, 0, -6]      , "-6x^3"           )
             , (P [5, 5, 5, 5, 5, 5] , "5x^5 + 5x^4 + 5x^3 + 5x^2 + 5x + 5")
             ]
           , testF1 "show test float" show
             [ (P [1.234, 2.345, 4.567], "4.567x^2 + 2.345x + 1.234")
             ]
           ]

-- Exercise 4 -----------------------------------------
ex4Tests :: [Test]
ex4Tests = [ testF2 "plus poly test" plus
             [ (P [1, 0, 1]     , P [1, 1]      , P [2, 1, 1])
             , (P [5, 5, 5, 5]  , P [1, 0, 1]   , P [6, 5, 6, 5])
             , (P [1, 0, 0, 2]  , P [0, -1, 2]  , P [1, -1, 2, 2])
             , (P [-4, 0, 0, 0] , P [-1, -1, -2], P [-5, -1, -2])
             , (P [0, 0, 0, -6] , P [0, 0, 0, 0], P [0, 0, 0, -6])
             ]
           ]

-- Exercise 5 -----------------------------------------
ex5Tests :: [Test]
ex5Tests = [ testF2 "times poly test" times
             [ (P [1, 0, 1]     , P [1, 1]      , P [1, 1, 1, 1])
             , (P [5, 5, 5, 5]  , P [1, 0, 1]   , P [5, 5, 10, 10, 5, 5])
             , (P [1, 2]        , P [0, -1, 2]  , P [0, -1, 0, 4])
             , (P [-4, 0, 0, 0] , P [-1, -1, -2], P [4, 4, 8])
             , (P [0, 0, 0, -6] , P [0, 0, 0, 0], P [])
             ]
           ]
-- Exercise 6 -----------------------------------------
ex6Tests :: [Test]
ex6Tests = [ testF2 "plus minus test" (-)
             [ (P [1, 0, 1]     , P [1, 1]      , P [0, -1, 1])
             , (P [5, 5, 5, 5]  , P [1, 0, 1]   , P [4, 5, 4, 5])
             , (P [1, 0, 0, 2]  , P [0, -1, 2]  , P [1, 1, -2, 2])
             , (P [-4, 0, 0, 0] , P [-1, -1, -2], P [-3, 1, 2])
             , (P [0, 0, 0, -6] , P [0, 0, 0, 0], P [0, 0, 0, -6])
             ]
           ]

-- Exercise 7 -----------------------------------------
ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
             [ (2*x^3 + 1       , 2   , 17    )
             , (2*x^2 - x       , 2   , 6     )
             , (-4              , 10  , -4    )
             , (-2*x^2 - x - 1  , 10  , -211  )
             , (2*x^2 + 2*x - 1 , -10 , 179   )
             , (2*x^2 - 2*x     , 2   , 4     )
             , (-6*x^3          , 2   , -48   )
             , (5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5, -2, 55)
             ]
           , testF2 "applyP test float" applyP
             [ (-2*x^2 - x - 1  , 10.5  , -232.0)
             , (2*x^2 + 2*x - 1 , -10.1 , 182.82)
             ]
           ]

-- Exercise 8 & 9 -----------------------------------------
ex8Tests :: [Test]
ex8Tests = [ testF1 "derivative" deriv
             [ (2*x^3 + 1         , 6*x^2         )
             , (2*x^2 - x         , 4*x - 1       )
             , (-4                , 0             )
             , (-2*x^2 - x - 1    , -4*x - 1      )
             , (2*x^4 + 2*x^3 - 1 , 8*x^3 + 6*x^2 )
             , (2*x^2 - 2*x       , 4*x - 2       )
             , (4*x - 2           , 4             )
             , (5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5, 20*x^3 + 15*x^2 + 10*x + 5)
             ]
           , testF2 "n-derivative" nderiv
             [ (5, 5*x^5 + 5*x^4 + 5*x^3 + 5*x^2 + 5*x + 5, 600 )
             , (1, 2*x^2 - 2*x, 4*x - 2                           )
             , (2, 2*x^2 - 2*x, 4                                 )
             , (2, 2*x^2 - 2*x, deriv $ deriv (2*x^2 - 2*x)       )
             , (3, 2*x^2 - 2*x, 0                                 )
             , (-1, 2*x^2 - 2*x, 0                                )
             ]
           ]

-- All Tests -----------------------------------------
allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , ex8Tests
                  ]
