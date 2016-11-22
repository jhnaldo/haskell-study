{-# OPTIONS_GHC -Werror #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------
-- Domain (Complete Lattice)
------------------------------------------------
class CompleteLattice lattice where  
  -- alpha
  alpha :: Set a -> lattice a

  -- gamma
  -- gamma :: lattice a -> Set a

  -- top
  (⊤) :: lattice a

  -- bottom
  (⊥) :: lattice a

  -- partial order
  (⊏) :: (Eq a) => lattice a -> lattice a -> Bool

  -- join
  (⊔) :: (Eq a) => lattice a -> lattice a -> lattice a

  -- meet
  (⊓) :: (Eq a) => lattice a -> lattice a -> lattice a

  -- not partial order
  (⊏/) :: (Eq a) => lattice a -> lattice a -> Bool
  x ⊏/ y = x ⊏ y

------------------------------------------------
-- Simple Domain
------------------------------------------------
data SimpleDomain a = SimpleTop | SimpleBot
instance CompleteLattice SimpleDomain where
  -- alpha
  alpha set
    | null set  = SimpleBot
    | otherwise = SimpleTop

  -- top
  (⊤) = SimpleTop

  -- bottom
  (⊥) = SimpleBot

  -- partial order
  SimpleTop ⊏ SimpleBot = False
  _ ⊏ _                 = True

  -- join
  SimpleBot ⊔ SimpleBot = SimpleBot
  _ ⊔ _                 = SimpleTop

  -- meet
  SimpleTop ⊓ SimpleTop = SimpleTop
  _ ⊓ _                 = SimpleBot


------------------------------------------------
-- Flat Domain
------------------------------------------------
data FlatDomain a = FlatMany | FlatOne a | FlatZero
instance CompleteLattice FlatDomain where
  -- alpha
  alpha set
    | null set          = FlatZero
    | Set.size set == 1 = FlatOne $ head $ Set.toList set
    | otherwise         = FlatMany

  -- top
  (⊤) = FlatMany

  -- bottom
  (⊥) = FlatZero

  -- partial order
  FlatZero ⊏ _ = True
  _ ⊏ FlatMany = True
  FlatMany ⊏ _ = False
  _ ⊏ FlatZero = True
  (FlatOne x) ⊏ (FlatOne y)
    | x == y    = True
    | otherwise = False

  -- join
  FlatMany ⊔ _     = FlatMany
  _ ⊔ FlatMany     = FlatMany
  FlatZero ⊔ other = other
  other ⊔ FlatZero = other
  (FlatOne x) ⊔ (FlatOne y)
    | x == y    = (FlatOne x)
    | otherwise = FlatMany

  -- meet
  FlatMany ⊓ other = other
  other ⊓ FlatMany = other
  FlatZero ⊓ _     = FlatZero
  _ ⊓ FlatZero     = FlatZero
  (FlatOne x) ⊓ (FlatOne y)
    | x == y    = (FlatOne x)
    | otherwise = FlatZero

main :: IO ()
main = undefined
