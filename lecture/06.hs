module Lecture06 where

{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------
-- Being Lazy with Class
------------------------------------------------------------------------
-- Strict evaluation
-- Side effects and purity
-- Lazy evaluation
-- Pattern matching drives evaluation
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

-- Consequences
badSum :: Num a => [a] -> a
badSum []     = 0
badSum (x:xs) = x + badSum xs

lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs

strictSum :: Num a => [a] -> a
strictSum = go 0
  where go acc []     = acc
        go acc (x:xs) = acc `seq` go (x + acc) xs

strictSum' :: Num a => [a] -> a
strictSum' = go 0
  where go acc []      = acc
        go !acc (x:xs) = go (x + acc) xs

-- Short-circuiting operators
(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False

-- User-defined control structures
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- Infinite data structures
withIndices :: [a] -> [(a,Integer)]
withIndices xs = zip xs [0..]

nats :: [Integer]
nats = 0 : map (+1) nats

-- Pipelining/wholemeal programming

------------------------------------------------------------------------
-- Being Lazy with Class
------------------------------------------------------------------------
