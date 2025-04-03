module Exercises where

import Data.List (elemIndex, lookup)
import Test.HUnit (Test (TestList), (~:), (~?=))
import Prelude hiding (mapM)

{- Examples -}

locations :: [(String, String)]
locations =
  [ ("Wu & Chen Auditorium", "Levine 101"),
    ("Raisler Lounge", "Towne 225"),
    ("Irvine Auditorium", "")
  ]

{- Without using monad functions -}
getRoomNumber :: String -> Maybe String
getRoomNumber room =
  case lookup room locations of
    Nothing -> Nothing
    Just location ->
      case elemIndex ' ' location of
        Nothing -> Nothing
        Just i -> Just (drop (i + 1) location)

{- With monad functions -}
getRoomNumber' :: String -> Maybe String
getRoomNumber' room = do
  location <- lookup room locations
  i <- elemIndex ' ' location
  return (drop (i + 1) location)

{- Without using monad functions -}
crossProd :: [a] -> [b] -> [(a, b)]
crossProd xs ys =
  concatMap
    ( \x ->
        concatMap
          (\y -> [(x, y)])
          ys
    )
    xs

{- With monad functions -}
crossProd' :: [a] -> [b] -> [(a, b)]
crossProd' xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-------

{-
Exercise:
Get the third element of a list. Use safeHead and safeTail.
e.g. [1, 2, 3, 4] -> Just 3 and [1, 2] -> Nothing.

Write at least three test cases for your code.
(One value of type `Test` constructed using a list of three tests counts as
three cases, so just fill in cases in testSafeThird.)

Write the tests BEFORE completing the next exercise.
-}

safeThird :: [a] -> Maybe a
safeThird = error "unimplemented"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : as) = Just as

testSafeThird :: Test
testSafeThird =
  TestList
    [ error "unimplemented",
      error "unimplemented",
      error "unimplemented"
    ]

-------

{-
Exercise:
Compute the factors of a number. Use list comprehension.
e.g. 15 -> [1, 3, 5, 15]

Write at least three test cases for your code.
-}

factors :: Int -> [Int]
factors = error "unimplemented"

testFactors :: Test
testFactors =
  TestList
    [ error "unimplemented",
      error "unimplemented",
      error "unimplemented"
    ]

-------

{-
Exercise:
Implement mapM (for lists) using sequence (and another function).

Write at least three test cases for your code.
-}

mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM = error "unimplemented"

testMapM :: Test
testMapM =
  TestList
    [ error "unimplemented",
      error "unimplemented",
      error "unimplemented"
    ]

{-
For testFactors, make sure your tests covered non-positive inputs.
(0 has no factors for our purposes.)
-}