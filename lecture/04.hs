module Lecture04 where

------------------------------------------------------------------------
-- Type Classes
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

import Data.Char  ( isUpper, toUpper )
import Data.Maybe ( mapMaybe )
import Text.Read  ( readMaybe )

-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

-- class Blerg a b where
--   blerg :: a -> b -> Bool
-- 
-- class Extract a b | a -> b where
--   extract :: a -> b
-- 
-- instance Extract (a, b) a where
--   extract (x, y) = x


------------------------------------------------------------------------
-- Monoids
------------------------------------------------------------------------
-- class Monoid m where
--   mempty  :: m
--   mappend :: m -> m -> m
-- 
--   mconcat :: [m] -> m     -- this can be omitted from Monoid instances
--   mconcat = foldr mappend mempty
-- 
-- (<>) :: Monoid m => m -> m -> m    -- infix operator for convenience
-- (<>) = mappend

-- instance Monoid [a] where
--   mempty  = []
--   mappend = (++)

-- this is not the most efficient!
intInts :: Monoid m => (Integer -> m) -> m   -- interesting ints!
intInts mk_m = go [1..100]   -- [1..100] is the list of numbers from 1 to 100
  where go [] = mempty
        go (n:ns)
          | let div_by_5 = n `mod` 5 == 0
                div_by_7 = n `mod` 7 == 0
          , (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7))
          = mappend (mk_m n) (go ns)
          | otherwise
          = go ns

intIntsList :: [Integer]
intIntsList = intInts (:[])

data Product a = Product a
instance Num a => Monoid (Product a) where
  mempty                          = Product 1
  mappend (Product x) (Product y) = Product (x * y)

getProduct :: Product a -> a
getProduct (Product x) = x

intIntsProduct :: Integer
intIntsProduct = getProduct $ intInts Product

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
-- instance Functor Maybe where

--   fmap _ Nothing  = Nothing
--   fmap f (Just x) = Just (f x)
