-- CIS 194, Spring 2015
--
-- Test cases for HW 06

module HW06Tests where

import HW06
import Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF1 "fib test" fib
             [ (0, 1)
             , (1, 1)
             , (2, 2)
             , (3, 3)
             , (4, 5)
             , (5, 8)
             , (6, 13)
             ]
           ]

-- All Tests ------------------------------------------
