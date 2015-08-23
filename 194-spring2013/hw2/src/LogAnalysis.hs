{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Log

-- exercise 1 ---------------
parseMessage :: String -> LogMessage
parseMessage "" = Unknown "empty log message"
parseMessage lMsg = case words lMsg of
        ("I":ts:msg)    ->  LogMessage Info (read ts) (unwords msg)
        ("W":ts:msg)    ->  LogMessage Warning (read ts) (unwords msg)
        ("E":pr:ts:msg) ->  LogMessage (Error (read pr)) (read ts) (unwords msg)
        _               -> Unknown lMsg

parse :: String -> [LogMessage]
parse logStr = map parseMessage $ lines logStr

-- exercise 2 ---------------
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msgNew@(LogMessage _ tsNew _) (Node lt msg@(LogMessage _ ts _ ) rt)
        | ts == tsNew = Node lt msgNew rt
        | ts < tsNew  = Node lt msg (insert msgNew rt)
        | otherwise   = Node (insert msgNew lt) msg rt
insert _ mTree = mTree

-- exercise 3 ---------------
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- exercise 4 ---------------
inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node lt msg rt) = inOrder lt ++ [msg] ++ inOrder rt

-- exercise 5 ---------------
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = ["empty log file"]
whatWentWrong msgs = map (\(LogMessage _ _ msg) -> msg) eGT49Sorted
    where
      eGT49 =
          filter (\(LogMessage (Error n) _ _) -> n > 49) $
          filter (\(LogMessage mType _ _ ) -> mType /= Info && mType /= Warning) msgs
      eGT49Sorted = inOrder $ build eGT49


-- to verify whatWentWrong is correct
testSolution :: FilePath -> IO [String]
testSolution fp = do
    logFile <- readFile fp
    let e = filter (\x -> take 1 (words x) == ["E"]) $ lines logFile
        eGT49 = filter (\x -> (read $ words x !! 1 ::Int) > 49) e
        s = map (\x -> (read $ words x !! 2 ::Int, unwords $ drop 3 $ words x)) eGT49
        s' = sortBy (compare `on` fst) s
    return (map snd s')
