-- CIS 194, Spring 2015
--
-- Test cases for HW 03

module HW03Tests where

import HW03
import Testing


-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF3 "extend test" (extend st2)
             [ ("c", 2, "c", 2)
             , ("a", 12, "b", 42532)
             , ("e", 12, "a", 23452)
             , ("b", 5, "e", 0)
             ]
           , testF1 "empty test" empty
             [ ("a", 0), ("b", 0), ("c", 0)
             ]
           ]
  where
    st2 = extend st1 "b" 42532
    st1 = extend empty "a" 23452

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF1 "evalE test" (evalE empty)
             [ ((Val 5), 5)
             , ((Op (Val 1) Eql (Val 2)), 0)
             , ((Op (Var "a") Plus (Val 10)), 10)
             , ((Op (Val 5) Minus (Val 3)), 2)
             , ((Op (Val 2) Times (Val 8)), 16)
             , ((Op (Val 9) Divide (Val 2)), 4)
             , ((Op (Val 2) Gt (Val 1)), 1)
             , ((Op (Val 2) Gt (Val 2)), 0)
             , ((Op (Val 2) Ge (Val 2)), 1)
             , ((Op (Val 2) Lt (Val 3)), 1)
             , ((Op (Val 2) Lt (Val 2)), 0)
             , ((Op (Val 2) Le (Val 2)), 1)
             , ((Op (Val 2) Eql (Val 2)), 1)
             , ((Op (Val 10) Eql (Val 2)), 0)
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "desugar test" desugar
             [ ((Incr "A"), DAssign "A" $ Op (Var "A") Plus (Val 1))
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "evalSimple test" (evalSimple empty)
             [ ((DAssign "A" (Val 10)), "A", 10)
             ]
           , testF2 "run test" runHelper
             [ (factorial, 1, 1)
             , (factorial, 2, 2)
             , (factorial, 3, 6)
             , (factorial, 4, 24)
             , (factorial, 10, 3628800)
             , (squareRoot, 9, 3)
             , (squareRoot, 14, 3)
             , (squareRoot, 121, 11)
             , (squareRoot, 4321, 65)
             , (fibonacci, 0, 1)
             , (fibonacci, 1, 1)
             , (fibonacci, 2, 2)
             , (fibonacci, 3, 3)
             , (fibonacci, 4, 5)
             , (fibonacci, 5, 8)
             , (fibonacci, 6, 13)
             , (fibonacci, 7, 21)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  ]
