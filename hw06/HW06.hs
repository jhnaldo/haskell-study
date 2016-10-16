{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = convert [0..]
  where
    convert [] = []
    convert (k : lst) = fib k : convert lst

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = convert 1 1
  where
    convert :: Integer -> Integer -> [Integer]
    convert x y = x : (convert y $ x + y)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x stream) = x : streamToList stream

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x stream) = Cons (f x) $ fmap f stream

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f $ f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x left) right = Cons x $ sInterleave right left

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake k (Cons x rest) = x : sTake (k - 1) rest

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate succ 0

ruler :: Stream Integer
ruler = helper 0
  where
    helper k = sInterleave (sRepeat k) $ helper $ k + 1

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons seed $ rand $ (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x : xs) = Just $ helper (x, x) xs
  where
    helper (l, r) [] = (l, r)
    helper (l, r) (n : remain) = helper (min l n, max r n) remain

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = M Integer Integer Integer Integer

plus :: Matrix -> Matrix -> Matrix
plus (M a b c d) (M o p q r) = M (a + o) (b + p) (c + q) (d + r)

times :: Matrix -> Matrix -> Matrix
times (M a b c d) (M o p q r) = M  w x y z
  where
    w = a * o + b * q
    x = a * p + b * r
    y = c * o + d * q
    z = c * p + d * r

timesK :: Integer -> Matrix -> Matrix
timesK k (M a b c d) = M (a * k) (b * k) (c * k) (d * k)

matI :: Matrix
matI = M 1 0 0 1

instance Num Matrix where
    (+) = plus
    (*) = times
    negate = timesK (-1)
    fromInteger k = timesK k matI
    -- No meaningful definitions exist
    abs _ = undefined
    signum _ = undefined

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib 1 = 1
fastFib k = get $ (M 1 1 1 0) ^ k
  where
    get (M x _ _ _) = x

instance Show Matrix where
    show (M a b c d) = "[" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ ", " ++ (show d) ++ "]"
