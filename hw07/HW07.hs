{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = [ f x | x <- m ]

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV x y vec = liftM2 assign (vec !? x) (vec !? y)
    where assign xv yv = vec // [(x, yv), (y, xv)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f lst = sequence $ map f lst

getElts :: [Int] -> Vector a -> Maybe [a]
getElts lst vec = mapM (vec !?) lst

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec = liftM getElem $ getRandomR (0, len - 1)
  where
    getElem x | len == 0  = Nothing
              | otherwise = Just $ vec ! x
    len = length vec

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = liftM V.fromList $ replicateM len getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len range = liftM V.fromList $ replicateM len $ getRandomR range

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = sub vec $ len - 1
  where
    sub v 0 = return v
    sub v i = do
        newV <- liftM (vswap v i) $ getRandomR (0, i)
        sub newV $ i - 1
    vswap v i j = v // [(i, v ! j), (j, v ! i)]
    len = length vec

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec pivot = (less, k, greater)
  where
    less = V.filter (k >) dropped
    greater = V.filter (k <=) dropped
    dropped = lvec <> rvec
    lvec = V.slice 0 pivot vec
    rvec = V.slice (pivot + 1) (len - pivot - 1) vec
    k = vec ! pivot
    len = V.length vec

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec = sub $ vec !? 0
  where
    sub Nothing = V.fromList []
    sub (Just _) = (qsort left) <> (cons pivot $ qsort right)
    (left, pivot, right) = partitionAt vec 0

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec
    | len > 0 = do
        pos <- getRandomR (0, len - 1)
        let (left, pivot, right) = partitionAt vec pos
        lvec <- qsortR left
        rvec <- qsortR right
        return $ lvec <> (cons pivot rvec)
    | otherwise = return $ V.fromList []
  where
    len = length vec

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select idx vec = do
        pos <- getRandomR (0, len - 1)
        check idx $ sub $ partitionAt vec pos
  where
    len = length vec
    check idx result
        | idx < len = result
        | otherwise = return Nothing
    sub (left, pivot, right)
        | llen > idx = select idx left
        | llen < idx = select (idx - llen - 1) right
        | otherwise = return (Just pivot)
      where
        llen = length left

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit |
             suit <- suits,
             label <- labels
           ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck = do
        card <- deck !? 0
        return (card, V.slice 1 (len - 1) deck)
  where
    len = length deck

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 deck = (Just ([], deck))
getCards k deck = do
        (card, ndeck) <- nextCard deck
        (cards, remain) <- getCards (k - 1) ndeck
        return (card : cards, remain)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
