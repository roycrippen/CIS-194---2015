module Hw3 where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = Map String Int

state :: State -> String -> Int
state st name = fromMaybe 0 (Map.lookup name st)

-- Exercise 1 -----------------------------------------
extend :: State -> String -> Int -> State
extend st "" _ = st
extend st name value =
    case Map.lookup name st of
        Nothing -> Map.insert name value st
        _       -> Map.alter f name st
     where  f _ = Just value

empty :: State
empty = Map.fromList []

-- Exercise 2 -----------------------------------------
evalE :: State -> Expression -> Int
evalE st expr =
    case expr of
        Var s -> state st s
        Val n -> n
        Op exp1 bop exp2 ->
            case bop of
                Plus   -> evalE st exp1 + evalE st exp2
                Minus  -> evalE st exp1 - evalE st exp2
                Times  -> evalE st exp1 * evalE st exp2
                Divide -> if evalE st exp2 /= 0
                          then evalE st exp1 `div` evalE st exp2
                          else 0
                Gt     -> if evalE st exp1 >  evalE st exp2 then 1 else 0
                Ge     -> if evalE st exp1 >= evalE st exp2 then 1 else 0
                Lt     -> if evalE st exp1 <  evalE st exp2 then 1 else 0
                Le     -> if evalE st exp1 <= evalE st exp2 then 1 else 0
                Eql    -> if evalE st exp1 == evalE st exp2 then 1 else 0

-- Exercise 3 -----------------------------------------
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar statement =
    case statement of
        Assign s expr -> DAssign s expr
        Incr s -> desugar (Assign s (Op (Var s) Plus (Val 1)))
        If expr stm1 stm2 -> DIf expr (desugar stm1) (desugar stm2)
        While expr stm -> DWhile expr (desugar stm)
        For iAssignStm expr iIncStm stm -> desugar (Sequence iAssignStm (While expr (Sequence stm iIncStm)))
        Sequence stm1 stm2 -> DSequence (desugar stm1) (desugar stm2)
        Skip -> DSkip

-- Exercise 4 -----------------------------------------
evalSimple :: State -> DietStatement -> State
evalSimple st dst =
    case dst of
        DAssign s expr -> extend st s (evalE st expr)
        DIf expr stm1 stm2 -> if evalE st expr == 1
                              then evalSimple st stm1
                              else evalSimple st stm2
        DWhile expr stm -> if evalE st expr == 1
                           then evalSimple (evalSimple st stm) (DWhile expr stm)
                           else evalSimple st DSkip
        DSequence stm1 stm2 -> evalSimple (evalSimple st stm1) stm2
        DSkip -> st

run :: State -> Statement -> State
run st stm = evalSimple st (desugar stm)

-- Programs -------------------------------------------
slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))

squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 0)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
