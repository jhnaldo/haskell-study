module Lecture08 where

import Control.Monad
import Data.ByteString
import Data.Word

------------------------------------------------------------------------
-- Applicative Functors
------------------------------------------------------------------------
-- Motivation
-- type Name = String
-- 
-- data Employee = Employee { name    :: Name
--                          , phone   :: String }
--                 deriving Show

-- Generalizing
-- fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
-- fmap2 h fa fb = undefined

------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------
-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
-- 
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 h fa fb = (h `fmap` fa) <*> fb
-- 
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<$>) = fmap
-- 
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc
-- 
-- liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
-- liftX h fa b fc = h <$> fa <*> pure b <*> fc

------------------------------------------------------------------------
-- Applicative examples
------------------------------------------------------------------------
-- Maybe
-- instance Applicative Maybe where
--   pure              = Just
--   Nothing <*> _     = Nothing
--   _ <*> Nothing     = Nothing
--   Just f <*> Just x = Just (f x)

-- m_name1, m_name2 :: Maybe Name
-- m_name1 = Nothing
-- m_name2 = Just "Brent"
-- 
-- m_phone1, m_phone2 :: Maybe String
-- m_phone1 = Nothing
-- m_phone2 = Just "555-1234"
-- 
-- ex01 = Employee <$> m_name1 <*> m_phone1
-- ex02 = Employee <$> m_name1 <*> m_phone2
-- ex03 = Employee <$> m_name2 <*> m_phone1
-- ex04 = Employee <$> m_name2 <*> m_phone2

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------
-- word :: Parser ByteString
-- word = PS.takeWhile $ inClass "a-zA-Z"
-- 
-- upper :: Parser Word8
-- upper = satisfy $ inClass "A-Z"
-- 
-- lword :: Parser ByteString
-- lword = PS.takeWhile (inClass "a-z")
-- 
-- name :: Parser ByteString
-- name = BS.cons <$> upper <*> lword
-- 
-- name' :: Parser ByteString
-- name' = liftA2 BS.cons upper lword
-- 
-- skipSpace :: Parser ()
-- skipSpace = skipWhile isSpace_w8
-- 
-- firstLast :: Parser (ByteString, ByteString)
-- firstLast = (,) <$> name <*> (skipSpace *> name)
-- 
-- data Name' = TwoName ByteString ByteString
--           | ThreeName ByteString ByteString ByteString

------------------------------------------------------------------------
-- Monads to the Rescue!
------------------------------------------------------------------------
-- class Monad m where
--     return :: a -> m a
--     (>>=)  :: m a -> (a -> m b) -> m b
-- 
-- firstLast' :: Parser (ByteString, ByteString)
-- firstLast' = do
--   fname <- name
--   lname <- skipSpace *> name
--   return (fname, lname)
-- 
-- fullName :: Parser Name'
-- fullName = do
--   n1 <- name
--   n2 <- skipSpace *> name
--   mn <- skipSpace *> optional name
--   case mn of
--     Just n3 -> return $ ThreeName n1 n2 n3
--     Nothing -> return $ TwoName n1 n2
-- 
-- bool :: Parser Bool
-- bool = do
--   s <- word
--   case s of
--     "true"  -> return True
--     "false" -> return False
--     _       -> fail $ show s ++ " is not a bool"
-- 
-- list :: Parser a -> Parser [a]
-- list p = char '(' *> sepBy p comma <* char ')'
--     where comma = skipSpace *> char ',' <* skipSpace
-- 
-- boolList :: Parser [Bool]
-- boolList = list bool
-- 
-- names :: Parser [Name']
-- names = boolList >>= mapM bToP
--   where bToP True  = ThreeName <$> sn <*> sn <*> sn
--         bToP False = TwoName   <$> sn <*> sn
--         sn = skipSpace *> name
