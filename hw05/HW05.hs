{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as C

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
  vics <- parseFile vicsPth
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

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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

