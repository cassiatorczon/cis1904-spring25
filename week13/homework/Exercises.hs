{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Exercises where

import BST
import Test.QuickCheck

{- Read [instructions.md] first. -}

-- Exercise 1

instance Arbitrary Tree where
  arbitrary :: Gen Tree
  arbitrary = sized (\n -> genTree (-n) n)
    where
      genTree :: Int -> Int -> Gen Tree
      genTree lower upper
        | lower > upper = error "unimplemented"
        | otherwise = error "unimplemented"

prop_arbitraryValid :: Tree -> Bool
prop_arbitraryValid = isBST

-- Exercise 2

find :: Int -> Tree -> Bool
find = findGood

prop_findPostPresent :: Int -> Tree -> Bool
prop_findPostPresent = error "unimplemented"

prop_findPostAbsent :: Int -> Tree -> Bool
prop_findPostAbsent = error "unimplemented"

-- Exercise 3

bad1Present :: Bool
bad1Present = error "unimplemented"

bad1Absent :: Bool
bad1Absent = error "unimplemented"

bad2Present :: Bool
bad2Present = error "unimplemented"

bad2Absent :: Bool
bad2Absent = error "unimplemented"

bad3Present :: Bool
bad3Present = error "unimplemented"

bad3Absent :: Bool
bad3Absent = error "unimplemented"

---- end of exercises ----

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"
