module Lecture07 where

import Control.Monad

------------------------------------------------------------------------
-- Monads
------------------------------------------------------------------------
-- Motivation
data Tree a = Node (Tree a) a (Tree a)
            | Empty
              deriving (Show)

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree1 _ (Node _ _ _) Empty = Nothing
zipTree1 _ Empty (Node _ _ _) = Nothing
zipTree1 _ Empty Empty        = Just Empty
zipTree1 f (Node l1 x r1) (Node l2 y r2) =
    case zipTree1 f l1 l2 of
      Nothing -> Nothing
      Just l  -> case zipTree1 f r1 r2 of
                   Nothing -> Nothing
                   Just r  -> Just $ Node l (f x y) r

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
                   Nothing -> Nothing
                   Just x  -> f x

zipTree2 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree2 _ (Node _ _ _) Empty = Nothing
zipTree2 _ Empty (Node _ _ _) = Nothing
zipTree2 _ Empty Empty        = Just Empty
zipTree2 f (Node l1 x r1) (Node l2 y r2) =
    bindMaybe (zipTree2 f l1 l2) $ \l ->
        bindMaybe (zipTree2 f r1 r2) $ \r ->
            Just (Node l (f x y) r)

-- Monad
-- Examples
zipTree3 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree3 _ (Node _ _ _) Empty = Nothing
zipTree3 _ Empty (Node _ _ _) = Nothing
zipTree3 _ Empty Empty        = Just Empty
zipTree3 f (Node l1 x r1) (Node l2 y r2) =
    zipTree3 f l1 l2 >>= \l ->
        zipTree3 f r1 r2 >>= \r ->
            return (Node l (f x y) r)

addM :: Monad m => m Int -> m Int -> m Int
addM mx my = do
  x <- mx
  y <- my
  return $ x + y

addM' :: Monad m => m Int -> m Int -> m Int
addM' mx my = mx >>= \x -> my >>= \y -> return (x + y)

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ (Node _ _ _) Empty = Nothing
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ Empty Empty        = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) = do
    l <- zipTree f l1 l2
    r <- zipTree f r1 r2
    return $ Node l (f x y) r

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

ex04 = do
  checked <- check 7
  halve checked
ex05 = do
  checked <- check 12
  halve checked
ex06 = do
  halved <- halve 12
  check halved

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex07 = [10,20,30] >>= addOneOrTwo
ex08 = do
  num <- [10, 20, 30]
  addOneOrTwo num

ex09 = addOneOrTwo =<< [10,20,30]

ex10 = do
  num <- [1..20]
  guard (even num)
  guard (num `mod` 3 == 0)
  return num

-- Monad combinators

-- List comprehensions
evensUpTo100 :: [Int]
evensUpTo100 = [ n | n <- [1..100], even n ]

oddPerfectSquares :: [Int]
oddPerfectSquares = [ n | n <- [1..100]
                        , odd n
                        , root <- [1..10]
                        , root * root == n ]

cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct as bs = [ (a,b) | a <- as, b <- bs ]

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f as bs = [ f a b | a <- as, b <- bs ]

primes :: [Int]
primes = [ p | p <- [2..]
             , all ((/= 0) . (p `mod`)) [2..p-1] ]
