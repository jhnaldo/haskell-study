{-# OPTIONS_GHC -Werror #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P (lx : ls)) (P (rx : rs)) = lx == rx && P ls == P rs
    (==) _ _ = False

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P lst) = subShow lst 0 ""
      where
        subShow [] _ str        = str
        subShow [0] 0 ""        = "0"
        subShow (0 : s) e str   = subShow s (e + 1) str
        subShow (1 : s) 0 ""    = subShow s 1 "1"
        subShow ((-1) : s) 0 "" = subShow s 1 "-1"
        subShow (c : s) e str   = subShow s (e + 1) $ (getC c) ++ (getX e) ++ (getS str)
        getC 1    = ""
        getC (-1) = "-"
        getC c    = (show c)
        getX 0   = ""
        getX 1   = "x"
        getX e   = "x^" ++ (show e)
        getS ""  = ""
        getS str = " + " ++ str

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P left) (P right) = P $ subPlus left right
  where
    subPlus [] right            = right
    subPlus left []             = left
    subPlus (lx : ls) (rx : rs) = (lx + rx) : subPlus ls rs

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P lst) right = foldl plus (P []) (toList lst 0)
  where
    toList [] _ = []
    toList (x : s) k = (prefix k $ mul x right) : (toList s $ k + 1)
    prefix 0 poly = poly
    prefix k (P lst) = prefix (k - 1) $ P $ 0 : lst
    mul x (P lst) = P $ map (* x) lst

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P lst) = P $ map negate lst
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs (P lst) = P $ map abs lst
    signum (P lst) = P $ map signum lst

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P lst) x = foldr subApply 0 lst
  where
    subApply k result = k + x * result

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 poly = poly
    nderiv k poly = nderiv (k - 1) $ deriv poly

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv poly @ (P []) = poly
    deriv (P (_ : lst)) = P $ subDeriv lst 1
      where
        subDeriv [] k = []
        subDeriv (x : lst) k = (x * k) : (subDeriv lst $ k + 1)
