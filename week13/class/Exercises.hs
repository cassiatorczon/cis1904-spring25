module Exercises where

import Data.List ((\\))
import Test.QuickCheck

-- Source: https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
sort :: [Int] -> [Int]
sort [] = []
sort (p : xs) = sort lesser ++ [p] ++ sort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

-- Bad alternative #1.
-- sort :: [Int] -> [Int]
-- sort = id

-- Bad alternative #2.
-- sort :: [Int] -> [Int]
-- sort [] = []
-- sort (p : xs) = sort lesser ++ [p] ++ sort greater
--   where
--     lesser = filter (< p) xs
--     greater = filter (> p) xs

prop_SortOrdered :: [Int] -> Bool
prop_SortOrdered xs = ordered (sort xs)

ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x1 : x2 : xs) = x1 <= x2 && ordered (x2 : xs)

-- Exercise: Write a property that catches the bug in #2.

--------

data Expr
  = Num Int
  | Add Expr Expr
  deriving (Show)

-- Exercise: Write an Arbitrary instance for Expr.

--------

eval :: Expr -> Int
eval (Num x) = x
eval (Add e1 e2) = eval e1 + eval e2

-- write property-based tests to check that, as interpreted by eval, `Add` is commutative
-- and associative