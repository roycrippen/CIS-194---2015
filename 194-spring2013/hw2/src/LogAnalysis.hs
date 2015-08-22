{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Data.Function (on)
import           Data.List     (sortBy)
import           Log

-- exercise 1 ---------------
parseMessage :: String -> LogMessage
parseMessage "" = Unknown "empty log message"
parseMessage msg =
    case msgType of
        "I" ->  LogMessage Info
                           (read $ msgParts !! 1)
                           (unwords $ drop 2 msgParts)
        "W" ->  LogMessage Warning
                           (read $ msgParts !! 1)
                           (unwords $ drop 2 msgParts)
        "E" ->  LogMessage (Error (read $ msgParts !! 1))
                           (read $ msgParts !! 2)
                           (unwords $ drop 3 msgParts)
        _   -> Unknown msg
    where msgParts = words msg
          [msgType] = take 1 msgParts

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
          filter(\(LogMessage mType _ _ ) -> mType /= Info && mType /= Warning) msgs
      eGT49Sorted = inOrder $ build eGT49

testSolution :: FilePath -> IO [String]
testSolution fp = do
    logFile <- readFile fp
    let e = filter (\x -> head (words x) == "E") $ lines logFile
        toInt :: String -> Int
        toInt = read
        eGT49 = filter (\x -> toInt(words x !! 1) > 49) e
        s = map (\x -> (read (words x !! 2)::Int, unwords(drop 3 (words x)))) eGT49
        s' = sortBy (compare `on` fst) s
    return (map snd s')
