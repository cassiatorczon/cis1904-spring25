{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.List
import GHC.Natural (Natural)
import Test.HUnit

{- Read [instructions.md] first. -}

newtype Poly = P [Int]
  deriving (Eq) -- Erase this.

-- Exercise 1

-- Add the instance of Eq here.

exercise1 :: Test
exercise1 =
  "eq"
    ~: [ P [1, 2, 3] == P [1, 2, 3] ~?= True,
         P [1, 2] == P [1, 2, 3] ~?= False,
         P [1, 2, 0] == P [1, 2] ~?= True,
         P [] == P [0, 0] ~?= True,
         P [1, 0, 2] == P [1, 2] ~?= False,
         P [0, 1, 2] == P [1, 2] ~?= False
       ]

-- Exercise 2

instance Show Poly where
  show :: Poly -> String
  show = error "unimplemented"
    where
      showTerm :: Int -> Natural -> String
      showTerm = error "unimplemented"

exercise2 :: Test
exercise2 =
  "show"
    ~: [ show (P [1, 2, 3]) ~?= "1 + 2x + 3x^2",
         show (P [-1, 0]) ~?= "-1",
         show (P []) ~?= "0",
         show (P [0]) ~?= "0",
         show (P [5, 0, 2]) ~?= "5 + 2x^2",
         show (P [0, 4]) ~?= "4x"
       ]

-- Exercise 3

instance Num Poly where
  (+) :: Poly -> Poly -> Poly
  (+) = plus -- Implemented in Exercise 4

  (*) :: Poly -> Poly -> Poly
  (*) = times -- Implemented in Exercise 4

  negate :: Poly -> Poly
  negate = error "unimplemented" -- Implement here

  fromInteger :: Integer -> Poly
  fromInteger = error "unimplemented" -- Implement here

  {- Note: abs and signum do not really make sense for Poly.
     Ideally, we would use a Ring typeclass rather than the
     Num typeclass for it, to avoid having somewhat unnatural
     definitions for these functions. However, for historical
     reasons, it is very useful to have it be an instance of
     the Num typeclass, so we provide a definition of these
     required functions here. -}
  abs :: Poly -> Poly
  abs = id

  signum :: Poly -> Poly
  signum x = if x == P [] then P [] else P [1]

exercise3a :: Test
exercise3a =
  "negate"
    ~: [ negate (P [1, 2, 3]) ~?= P [-1, -2, -3],
         negate (P [-1, 0]) ~?= P [1, 0],
         negate (P []) ~?= P []
       ]

exercise3b :: Test
exercise3b =
  "fromInteger"
    ~: [ 3 ~?= P [3],
         0 ~?= P [0]
       ]

-- Exercise 4

plus :: Poly -> Poly -> Poly
plus = error "unimplemented"

exercise4 :: Test
exercise4 =
  "sum"
    ~: [ P [0, 1, 2] + P [1, 0, 2] ~?= P [1, 1, 4],
         P [5, 1] + P [1, 1, 3] ~?= P [6, 2, 3],
         P [1, 1, 3] + P [5, 1] ~?= P [6, 2, 3],
         P [] + P [] ~?= P []
       ]

-- Exercise 5

times :: Poly -> Poly -> Poly
times = error "unimplemented"

exercise5 :: Test
exercise5 =
  "times"
    ~: [ P [1, 1, 1] * P [2, 2] ~?= P [2, 4, 4, 2],
         P [2, 2] * P [1, 1, 1] ~?= P [2, 4, 4, 2],
         P [1, 2, 3] * P [4, 5, 6] ~?= P [4, 13, 28, 27, 18],
         P [] * P [1, 2, 3] ~?= P []
       ]

-- Tying it all together:

x :: Poly
x = P [0, 1]

final :: Test
final = (2 * x - 1) * (x + 2) == (2 * x ^ 2 + 3 * x - 2) ~?= True

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

check :: Test
check =
  TestCase
    ( assertBool
        "fill in a time and question"
        ( time >= 0
            && question /= ""
        )
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3a,
          exercise3b,
          exercise4,
          exercise5,
          final,
          check
        ]
  return ()
