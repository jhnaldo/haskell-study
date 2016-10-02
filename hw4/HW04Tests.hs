-- CIS 194, Spring 2015
--
-- Test cases for HW 04

module HW04Tests where

import HW04
import Testing


-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ Test "Eq-instance test" id
             [ P [1, 2, 3] == P [1, 2, 3]
             , P [1, 2] /= P [1, 2, 3]
             ]
           ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [ testF1 "Show-instance test" show
             [ (P [1, 0, 0, 2], "2x^3 + 1")
             , (P [0, -1, 2], "2x^2 + -x")
             , (P [0, 1, 0, 1], "x^3 + x")
             , (P [0], "0")
             ]
           ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [ testF2 "plus test" plus
             [ (P [5, 0, 1], P [1, 1, 2], P [6, 1, 3])
             , (P [1, 0, 1], P [1, 1], P [2, 1, 1])
             ]
           ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [ testF2 "times test" times
             [ (P [1, 1, 1], P [2, 2], P [2, 4, 4, 2])
             ]
           ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [ testF1 "negate test" negate
             [ (P [1, 2, 3], P [-1, -2, -3])
             ]
           ]

-- Exercise 7 -----------------------------------------

ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
             [ (x^2 + 2*x + 1, 1, 4)
             , (x^2 + 2*x + 1, 2, 9)
             ]
           ]

-- Exercise 9 -----------------------------------------

ex9Tests :: [Test]
ex9Tests = [ testF1 "deriv test" deriv
             [ (x^2 + 3*x + 5, 2*x + 3)
             , (x^5 + 2*x^3 + 5*x, 5*x^4 + 6*x^2 + 5)
             ]
           ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  , ex7Tests
                  , ex9Tests
                  ]
