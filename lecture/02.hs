module Lecture02 where

------------------------------------------------------------------------
-- Additional Syntax
------------------------------------------------------------------------
strLength :: String -> Int
strLength []     = 0
strLength (_:xs) = let len_rest = strLength xs in
                   len_rest + 1

frob :: String -> Char
frob []  = 'a'   -- len is NOT in scope here
frob str
  | len > 5   = 'x'
  | len < 3   = 'y'
  | otherwise = 'z'
  where
    len = strLength str

sumTo20 :: [Int] -> Int
sumTo20 nums = go 0 nums   -- the acc. starts at 0
  where go :: Int -> [Int] -> Int
        go acc [] = acc   -- empty list: return the accumulated sum
        go acc (x:xs)
         | acc >= 20 = acc
         | otherwise = go (acc + x) xs

------------------------------------------------------------------------
-- Parametric polymorphism
------------------------------------------------------------------------
notEmpty :: [a] -> Bool
notEmpty (_:_) = True
notEmpty []    = False

limited :: a -> a
limited x = x

------------------------------------------------------------------------
-- Total and partial functions
------------------------------------------------------------------------
doStuff :: [Int] -> Int
doStuff []        = 0
doStuff [_]       = 0
doStuff (x1:x2:_) = x1 + x2

------------------------------------------------------------------------
-- Recursion patterns
------------------------------------------------------------------------
addOneToAll :: [Int] -> [Int]
addOneToAll []     = []
addOneToAll (x:xs) = x + 1 : addOneToAll xs

absAll :: [Int] -> [Int]
absAll []     = []
absAll (x:xs) = abs x : absAll xs

squareAll :: [Int] -> [Int]
squareAll []     = []
squareAll (x:xs) = x^2 : squareAll xs

-- exampleList = [-1, 2, 6]
-- map (+1) exampleList
-- map abs  exampleList
-- map (^2) exampleList

keepOnlyPositive :: [Int] -> [Int]
keepOnlyPositive [] = []
keepOnlyPositive (x:xs)
  | x > 0     = x : keepOnlyPositive xs
  | otherwise = keepOnlyPositive xs

keepOnlyEven :: [Int] -> [Int]
keepOnlyEven [] = []
keepOnlyEven (x:xs)
  | even x    = x : keepOnlyEven xs
  | otherwise = keepOnlyEven xs

fold :: (a -> b -> b) -> b  -> [a] -> b
fold f z []     = z
fold f z (x:xs) = f x (fold f z xs)

sum' :: [Int] -> Int
sum' = fold (+) 0

product' :: [Int] -> Int
product' = fold (*) 1

length' :: [a] -> Int
length'  = fold addOne 0
 where addOne _ s = 1 + s

------------------------------------------------------------------------
-- Functional Programming
------------------------------------------------------------------------
add1Mul4 :: [Int] -> [Int]
add1Mul4 x = map ((*4) . (+1)) x

negateNumEvens2 :: [Int] -> Int
negateNumEvens2 x = negate $ length $ filter even x

duplicate1 :: [String] -> [String]
duplicate1 = map dup
  where dup x = x ++ x

duplicate2 :: [String] -> [String]
duplicate2 = map (\x -> x ++ x)

------------------------------------------------------------------------
-- Currying and Partial Application
------------------------------------------------------------------------
f :: Int -> Int -> Int
f x y = 2*x + y

f' :: Int -> (Int -> Int)
f' x y = 2*x + y

f'' :: (Int,Int) -> Int
f'' (x,y) = 2*x + y

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

add' :: (Int, Int) -> Int
add' = uncurry (+)

add'' :: Int -> Int -> Int
add'' = curry add'

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map ((+2) . (*7)) . filter (>3)
