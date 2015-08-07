{-# LANGUAGE FlexibleInstances #-}

module Hw4 where
import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------
x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------
padZip :: [a] -> [a] -> a -> [(a, a)]
padZip xs1 xs2 v =
    case length xs1 - length xs2 of
        n | n < 0 -> zip (padList xs1 v (negate n)) xs2
        n | n > 0 -> zip xs1 (padList xs2 v n)
        _         -> zip xs1 xs2
    where padList :: [a] -> a -> Int -> [a]
          padList l val len = l ++ replicate len val

instance (Num a, Eq a) => Eq (Poly a) where
    P a  == P b  = fst(unzip $ padZip a b 0) == fst(unzip $ padZip b a 0)

--- Exercise 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a) = drop 3 (termStrLst $ reverse $ zip a [0..]) where
        termStrLst []      = []
        termStrLst (t:ts)  = termStr t ++ termStrLst ts where
            termStr (0, e) = case e of
                                 0 ->  if sum (map abs a) == 0 then " + 0" else ""
                                 _ -> ""
            termStr (c, 0) = " + " ++ show c
            termStr (c, 1) = case c of
                                 1  -> " + x"
                                 -1 -> " + -x"
                                 _  -> " + " ++ show c ++ "x"
            termStr (c, e) = case c of
                                 1  -> " + x^" ++ show e
                                 -1 -> " + -x^" ++ show e
                                 _  -> " + " ++ show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b)  =  P (plus' a b) where
    plus' [] p2 = p2
    plus' p1 [] = p1
    plus' p1 p2  = map sum . transpose $ [p1, p2]

-- Exercise 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b)  =  P (times' a b) where
    times' [] p2 = p2
    times' p1 [] = p1
    times' p1 p2 = map sum . transpose $ foil p1 p2 where
        foil [] _      = []
        foil (z:zs) f2 = map (* z) f2 : foil zs (0 : f2)

-- Exercise 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P (map (\z -> -z) a)
    fromInteger n = P [fromIntegral n]
    -- No meaningful definitions exist Num a =>
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P[]) _ = 0
applyP (P a) b = applyP' (reverse a) b where
    applyP' []     _  = 0
    applyP' [y]    _  = y
    applyP' (y:ys) v  = (y * v^length ys) + applyP' ys v

-- Exercise 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 a = a
    nderiv n a
        | n < 0     = 0   -- s/b error "negative n not allowed in nth derivative"
        | otherwise = nderiv (n - 1) (deriv a)

-- Exercise 9 -----------------------------------------
instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P [])     = P []
    deriv (P [_])    = P [0]
    deriv (P (_:as)) = P (fst $ unzip $ map (\(l,m) -> (l*m,m)) (zip as [1..]))
