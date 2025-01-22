module Exercises where

import Test.HUnit (Test (TestList), runTestTT, (~?=))

{-
Exercise 0: Uncomment the code below by removing the two hyphens.
It should give you a type error, e.g. a red underline under + with an error message
that appears when you hover. Comment out the code again when you've confirmed.
-}
-- foo = 1 + "hello"

{-
Exercise 1: Remove the comments that say [ORMOLU_DISABLE] and [ORMOLU_ENABLE].
When you save the file, the extra spaces before the 3 should disappear.
-}
{- ORMOLU_DISABLE -}
format :: Int
format =       3
{- ORMOLU_ENABLE -}

{-
Exercise 2: The following code should have an underline and an hlint suggestion
should appear when you hover. Click [Quick Fix], then [Apply hint "Redundant bracket"].
-}
linter :: Int
linter = (3 + 4)

{-
Exercise 3: Replace undefined with 1904.
-}
n :: Int
n = undefined

ntest :: Test
ntest = n ~?= 1904

main :: IO ()
main = do
  print "You have installed Haskell!"
  _ <- runTestTT $ TestList [ntest]
  return ()