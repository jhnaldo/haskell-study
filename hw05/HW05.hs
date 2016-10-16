{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origName newName = do
            orig <- BS.readFile origName
            new <- BS.readFile newName
            let origWords = BS.unpack orig
            let newWords = BS.unpack new
            let xorWords = map (uncurry Bits.xor) $ zip origWords newWords
            return . BS.pack $ filter (/= 0) xorWords

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key output = do
                enc <- BS.readFile $ output ++ ".enc"
                let encWords = BS.unpack enc
                let keyWords = BS.unpack key
                let resultWords = map (uncurry Bits.xor) $ zip encWords $ cycle keyWords
                BS.writeFile output $ BS.pack resultWords

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile fileName = do
            str <- BS.readFile fileName
            return $ decode str

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vicName tranName = do
            victim <- parseFile vicName :: IO (Maybe [TId])
            transaction <- parseFile tranName :: IO (Maybe [Transaction])
            let helper left right = case (left, right) of
                                        (Just vic, Just tran) -> Just $ filter (\x -> elem (tid x) vic) tran
                                        (_, _) -> Nothing
            return $ helper victim transaction

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (tran : rest) = Map.insertWith (+) (from tran) (negate $ amount tran) $ Map.insertWith (+) (to tran) (amount tran) $ getFlow rest

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal map = fst $ List.maximumBy comparePair $ Map.toList map
  where
    comparePair left right = compare (snd left) (snd right)

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs map tidList = getTran payerList payeeList tidList
  where
    getTran :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
    getTran [] _ _ = []
    getTran _ [] _ = []
    getTran ((_, 0) : prList) peList tids = getTran prList peList tids
    getTran prList ((_, 0) : peList) tids = getTran prList peList tids
    getTran ((prKey, prValue) : prList) ((peKey, peValue) : peList) tids =
      (Transaction prKey peKey delta $ head tids) : (getTran ((prKey, prValue - delta) : prList) peList $ tail tids)
      where
        delta = min prValue $ abs peValue
    payerList = List.sortBy (flip comparePair) $ filter (\p -> snd p > 0) pairList
    payeeList = List.sortBy comparePair $ filter (\p -> snd p < 0) pairList
    pairList = Map.toList map
    comparePair (_, lValue) (_, rValue) = compare lValue rValue

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON filename json = BS.writeFile filename $ encode json

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

