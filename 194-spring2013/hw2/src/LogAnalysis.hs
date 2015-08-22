{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Log

-- exercise 1 ---------------
parseMessage :: String -> LogMessage
parseMessage "" = Unknown "empty log message"
parseMessage msg = case msgType of
                        "I" ->  parseInfo msgParts
                        "W" ->  parseWarning msgParts
                        "E" ->  parseError msgParts
                        _   -> Unknown msg
                   where msgParts = words msg
                         [msgType] = take 1 msgParts

parseInfo :: [String] -> LogMessage
parseInfo (_:ts:msg) = LogMessage Info (read ts) (unwords msg)
parseInfo _          = LogMessage Info 0 "malformed info message"

parseWarning :: [String] -> LogMessage
parseWarning (_:ts:msg) = LogMessage Warning (read ts) (unwords msg)
parseWarning _          = LogMessage Warning 0 "malformed warning message"

parseError :: [String] -> LogMessage
parseError (_:pr:ts:msg) = LogMessage (Error (read pr)) (read ts) (unwords msg)
parseError _             = LogMessage (Error 0) 0 "malformed error message"

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
build msgs = build' msgs Leaf
    where build' :: [LogMessage] -> MessageTree -> MessageTree
          build' [] mt = mt
          build' [x] mt = insert x mt
          build' (x:xs) mt = build' xs (insert x mt)

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

default (Int)

testSolution :: FilePath -> IO [String]
testSolution fp = do
    logFile <- readFile fp
    let e = filter (\x -> take 1 (words x) == ["E"]) $ lines logFile
        eGT49 = filter (\x -> (read $ words x !! 1 ::Int) > 49) e
        s = map (\x -> (read $ words x !! 2 ::Int, unwords $ drop 3 $ words x)) eGT49
        s' = sortBy (compare `on` fst) s
    return (map snd s') 
