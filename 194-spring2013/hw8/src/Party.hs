{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import           Data.List   (sort)
import           Data.Monoid (Monoid, mappend, mconcat, mempty)
import           Data.Tree   (Tree (Node))
import           Employee (Employee (empName, empFun), GuestList (GL))

-- exercise 1 -----------------------------------
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

instance  Monoid (GuestList) where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2

-- exercise 2 -----------------------------------
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node t ts) = f t (map (treeFold f) ts)

-- exercise 3 -----------------------------------
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glTups = (withBoss, withoutBoss)
    where withBoss = glCons boss (mconcat $ map snd glTups)
          withoutBoss = mconcat $ map (uncurry moreFun) glTups

-- exercise 4 -----------------------------------
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- exercise 5 -----------------------------------
glPrintStr :: GuestList -> String
glPrintStr (GL emps fun) =
    "Total fun score of " ++ show fun ++ " from " ++ show (length emps) ++ " guests:\n\n"
     ++ (unlines . sort . map empName) emps
