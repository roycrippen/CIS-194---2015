{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import qualified ExprT    as T
import           Parser
import           StackVM

-- exercise 1 -----------------------------
eval :: T.ExprT -> Integer
eval (T.Lit i) = i
eval (T.Add e1 e2) = eval e1 + eval e2
eval (T.Mul e1 e2) = eval e1 * eval e2

-- exercise 2 -----------------------------
evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp T.Lit T.Add T.Mul s)

-- exercise 3 -----------------------------
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr T.ExprT where
    lit = T.Lit
    add = T.Add
    mul = T.Mul

reify :: T.ExprT -> T.ExprT
reify = id

-- exercise 4 -----------------------------
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit i = i > 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax $ a `max` b
    mul (MinMax a) (MinMax b) = MinMax $ a `min` b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

-- exercise 5 -----------------------------
instance Expr Program where
    lit i     = [PushI i]
    add e1 e2 = e1 ++ e2 ++ [Add]
    mul e1 e2 = e1 ++ e2 ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Maybe (Either String StackVal)
run s = fmap stackVM (compile s)

-- exercise 6 -----------------------------
data VarExprT = Lit' Integer
              | Add' VarExprT VarExprT
              | Mul' VarExprT VarExprT
              | Var' String
  deriving (Show, Eq)

class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var = Var'

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr VarExprT where
    lit = Lit'
    add = Add'
    mul = Mul'

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit a _ = Just a
    add v1 v2 m =  do
        l1 <- v1 m
        l2 <- v2 m
        return (l1 + l2)
    mul v1 v2 m =  do
        l1 <- v1 m
        l2 <- v2 m
        return (l1 * l2)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp' = exp' $ M.fromList vs









--
