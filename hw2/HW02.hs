{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches left right = length $ filter (uncurry (==)) $ zip left right

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\peg -> length $ filter (== peg) code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches left right = foldl minSum 0 pairs
  where
    pairs = zip (countColors left) (countColors right)
    minSum n (l, r) = n + min l r

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove left right = Move right exact nonExact
  where
    nonExact = matches left right - exact
    exact = exactMatches left right

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent move@(Move ans _ _) code = getMove code ans == move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n <= 0    = [[]]
  | otherwise = foldr merge [] colors
  where
    merge color lst = foldr (colorAppend color) lst $ allCodes $ n - 1
    colorAppend color code l = (color : code) : l

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = reverse $ subSolve [] $ allCodes $ len
  where
    subSolve _ [] = undefined
    subSolve lst (first : remain) = next lst (getMove code first) remain
    next lst move @ (Move _ l _) remain = case () of
      () | l == len -> move : lst
         | otherwise -> subSolve (move : lst) (filterCodes move remain)
    len = length code


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
