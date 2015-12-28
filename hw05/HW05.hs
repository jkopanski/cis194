{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List as List
import Parser
import Data.Bits

import Data.Functor ((<$>))

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret o m = do
  orig <- BS.readFile o
  modi <- BS.readFile m
  return $ BS.filter (/= 0) $ BS.pack $ BS.zipWith (xor) orig modi

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key f = do
  file <- BS.readFile $ f ++ ".enc"
  BS.writeFile f $ BS.pack $ zipWith (xor) (BS.unpack file) $ concat $ repeat (BS.unpack key)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = do
  tran <- BS.readFile f
  return $ decode tran

-- Exercise 4 -----------------------------------------
-- shamelessly copoied from Mattias Jakobsson
-- https://github.com/mjakob/cis194/blob/master/HW05/HW05.hs

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vicsPth tssPth = do
  Just vics <- parseFile vicsPth :: IO (Maybe [TId])
  let isBadTs :: Transaction -> Bool
      isBadTs (Transaction { tid = x }) = elem x vics
  tss <- parseFile tssPth
  return $ filter isBadTs <$> tss
--getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
--getBadTs fvict ftran = do
--  Just victims <- parseFile fvict
--  transactions <- parseFile ftran
--  let isBadTs :: Transaction -> Bool
--      isBadTs (Transaction { tid = x }) = x `elem` victims
--  return $ filter isBadTs <$> transactions

-- Exercise 5 -----------------------------------------

getStep :: Transaction -> Map String Integer
getStep t = Map.union (Map.singleton (from t) (-(amount t))) (Map.singleton (to t) (amount t))

getFlow :: [Transaction] -> Map String Integer
getFlow []     = Map.empty
getFlow (t:ts) = Map.unionWith (+) (getStep t) (getFlow ts)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = head $ Map.keys $ Map.filter (== v) m
  where v = maximum $ Map.elems m

-- Exercise 7 -----------------------------------------
sortMap :: Map String Integer -> [(String, Integer)]
sortMap m = List.sortBy (\ (_, v1) (_, v2) -> v2 `compare` v1) $ Map.toList m

payers :: Map String Integer -> [(String, Integer)]
payers m = sortMap $ Map.filter (< 0) m

payees :: Map String Integer -> [(String, Integer)]
payees m = sortMap $ Map.filter (> 0) m

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ts
  | Map.null m  = []
  | amount <= 0 = []
  | ts == []    = []
  | otherwise   = tr : (undoTs new_map $ tail ts)
  where
    payer = Map.foldrWithKey
      (\ name money (rname, rmoney) ->
        if rmoney < money
          then (name, money)
          else (rname, rmoney)) ("", 0) m
    payee = Map.foldrWithKey
      (\ name money (rname, rmoney) ->
        if rmoney > money
          then (name, money)
          else (rname, rmoney)) ("", 0) m
    amount = min (snd payer) (negate $ snd payee)
    tr = Transaction (fst payer) (fst payee) amount $ head ts
    new_map = Map.filter (/= 0)
            $ Map.adjust (amount +) (fst payee)
            $ Map.adjust ((-amount) +) (fst payer) m

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file ts = BS.writeFile file $ encode ts

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
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

