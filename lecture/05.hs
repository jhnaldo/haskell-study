module Lecture05 where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

------------------------------------------------------------------------
-- I/O
------------------------------------------------------------------------
-- Sequencing IO actions
sillyExchange :: IO ()
sillyExchange = do
  putStrLn "Hello, user!"
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"

-- IO types
-- putStrLn :: String -> IO ()
-- getLine  :: IO String

-- A slightly larger example
jabber :: IO ()
jabber = do
  wocky <- readFile "jabberwocky.txt"
  let wockylines = drop 2 (lines wocky)  -- discard title
  count <- printFirstLines wockylines
  putStrLn $ "There are " ++ show count ++ " stanzas in Jabberwocky."

printFirstLines :: [String] -> IO Int
printFirstLines ls = do
  let first_lines = extractFirstLines ls
  putStr (unlines first_lines)
  return $ length first_lines

extractFirstLines :: [String] -> [String]
extractFirstLines []         = []
extractFirstLines [_]        = []
extractFirstLines ("" : first : rest)
  = first : extractFirstLines rest
extractFirstLines (_ : rest) = extractFirstLines rest

------------------------------------------------------------------------
-- More Types!
------------------------------------------------------------------------
-- Record Syntax
data D = C { field1 :: Int, field2 :: String, field3 :: Char }

-- ByteStrings
-- import qualified Data.ByteString as BS
-- import Data.ByteString (ByteString)

-- IO is a Functor!
getWords :: IO [ByteString]
getWords = do
  ln <- BS.getLine
  return $ BS.split 32 ln -- 32 is the ASCII code for ' '
getWords' :: IO [ByteString]
getWords' = BS.split 32 <$> BS.getLine
