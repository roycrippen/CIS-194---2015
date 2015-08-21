module Hw5 ( getSecret
          , decryptWithKey
          , parseFile
          , getBadTs
          , getFlow
          , getCriminal
          , undoTs
          , writeJSON
          , doEverything
          ) where

import           Control.Arrow
import           Data.Bits            (xor)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (append, concat, length, pack,
                                             readFile, take, writeFile, zipWith)
import           Data.Function        (on)
import           Data.List            (groupBy, sort, sortBy)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map (elems, filter, fromList, keys,
                                              toList)
import           Parser

-- Exercise 1 -----------------------------------------
getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fp1 fp2 = do
    origDog <- BS.readFile fp1
    badDog  <- BS.readFile fp2
    let zipDogs = BS.zipWith xor badDog origDog
        msg = filter (/= 0) zipDogs
    return $ BS.pack msg

-- Exercise 2 -----------------------------------------
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey msg fp  = do
    victimsEnc <- BS.readFile $ fp ++ ".enc"
    let len = BS.length victimsEnc
        key = BS.append nMsg $ BS.take (len `mod` 17) msg
        nMsg = BS.concat $ replicate (fromIntegral len `div` 17) msg
        bsVictims  = BS.zipWith xor victimsEnc key
    BS.writeFile fp $ BS.pack bsVictims

-- Exercise 3 -----------------------------------------
parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fp = do
    bsRaw <- BS.readFile fp
    return $ decode bsRaw

-- Exercise 4 -----------------------------------------
getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs fVictims fTransactions = do
    vics  <- parseFile fVictims
    trans <- parseFile fTransactions
    case (vics, trans) of
        (Nothing, _)     -> return Nothing
        (_, Nothing)     -> return Nothing
        (Just v, Just t) -> return $ Just $ filter (\x -> tid x `elem` v) t

-- Exercise 5 -----------------------------------------
getFlow :: [Transaction] -> Map String Integer
getFlow lst = Map.fromList filterFlow
    where buildFlow = map (\x -> (from x, -(amount x))) lst ++ map (to &&& amount) lst
      --  buildFlow = map (\x -> (from x, -(amount x))) lst ++ map (\x -> (to x, amount x)) lst
          groupFlow = groupBy ((==) `on` fst) $ sort buildFlow
          sumFlow = map (\x -> (fst $ head x , sum [z | (_,z) <- x])) groupFlow
          filterFlow = filter (\x -> snd x /= 0) sumFlow

-- Exercise 6 -----------------------------------------
getCriminal :: Map String Integer -> String
getCriminal flowMap = head $ Map.keys $ Map.filter (== maxMoney) flowMap
    where maxMoney = maximum $ Map.elems flowMap

-- Exercise 7 -----------------------------------------
undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flowMap tidLst = addTID tidLst . reverse $ undoTs' payers payees []
    where payees = Map.toList $ Map.filter ( < 0 ) flowMap
          payers = Map.toList $ Map.filter ( > 0 ) flowMap

          addTID :: [TId] -> [Transaction] -> [Transaction]
          addTID tids trans =
              zipWith (\t tr -> tr {tid = t}) (take (length trans) tids) trans

          undoTs' :: [(String, Integer)] -> [(String, Integer)] -> [Transaction] -> [Transaction]
          undoTs' [] [] xs = xs
          undoTs' [] _  _  = error "empty payers list in undoTs'"
          undoTs' _  [] _  = error "empty payees list in undoTs'"
          undoTs' ers ees lst =
              let payers' = sortBy (compare `on` snd) ers
                  payees' = sortBy (compare `on` snd) ees
              in case (payers', payees') of
                  ([], _)      -> error "empty payer list in undoTs'"
                  (_, [])      -> error "empty payee list in undoTs'"
                  (x:xs, y:ys) ->
                      let n = snd x + snd y
                      in if n == 0
                         then undoTs' xs ys [ Transaction
                                                    { to = fst y
                                                    , from = fst x
                                                    , amount = snd x
                                                    , tid = ""
                                                    }
                                                ] ++ lst
                         else
                             if  n < 0
                             then undoTs' xs ((fst y, n) : ys) [ Transaction
                                                                    { to = fst y
                                                                    , from = fst x
                                                                    , amount = snd x
                                                                    , tid = ""
                                                                    }
                                                                 ] ++ lst
                              else undoTs' ((fst x, n) : xs) ys [ Transaction
                                                                  { to = fst y
                                                                  , from = fst x
                                                                  , amount = -(snd y)
                                                                  , tid = ""
                                                                  }
                                                               ] ++ lst

-- Exercise 8 -----------------------------------------
writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp newTrans = BS.writeFile fp $ encodePretty newTrans

-- Exercise 9 -----------------------------------------
doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
    key <- getSecret dog1 dog2
    decryptWithKey key vict
    mts <- getBadTs vict trans
    case mts of
        Nothing -> error "No Transactions"
        Just ts -> do
            mids <- parseFile fids :: IO (Maybe [TId])
            case mids of
                Nothing  -> error "No ids"
                Just ids -> do
                    let flow = getFlow ts
                    writeJSON out $ undoTs flow ids
                    return (getCriminal flow)
