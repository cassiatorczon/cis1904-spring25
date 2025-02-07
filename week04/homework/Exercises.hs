module Exercises where

{- Don't change the imports. -}
import Test.HUnit
import Prelude hiding (all, and, minimum)

{- Read [instructions.md] first. You *must* follow the constraints
   outlined there. In particular, you may *not* directly use recursion in this
   homework. (You will use it indirectly, by using map, filter, and foldr.)

   Please double check that you have precisely the `homework` folder open!
   You should see a type error in the definition of `foo` and *no*
   warnings or errors in the definition of `bar`. Make sure both of these things
   are true before continuing. Comment out `foo` and `bar` when you've confirmed.
-}

foo :: Int -> String
foo x = x + "hello"

bar :: Int -> String
bar x = foo x

-- Exercise -7:

-- Remove all 7s.

remove7 :: [[Int]] -> [[Int]]
remove7 = error "unimplemented"

exercise7 :: Test
exercise7 =
  "remove7"
    ~: [ remove7
           [ [7, 1, 2, 7],
             [3, 7],
             [4, 5, 6],
             [7, 7, 7]
           ]
           ~?= [ [1, 2],
                 [3],
                 [4, 5, 6],
                 []
               ]
       ]

-- Exercise 1:

-- Check if all elements satisfy a condition. Use `foldr`.

all :: (a -> Bool) -> [a] -> Bool
all = error "unimplemented"

-- Check if a list of lists is square. Use the `all` function you just defined.

square :: [[a]] -> Bool
square = error "unimplemented"

exercise1a :: Test
exercise1a =
  "all"
    ~: [ all id [] ~?= True,
         all even [2, 4] ~?= True,
         all even [1, 2, 3] ~?= False,
         all not [False, False] ~?= True
       ]

exercise1b :: Test
exercise1b =
  "square"
    ~: [ square jagged ~?= False,
         square perfect ~?= True,
         square [] ~?= True,
         square [[]] ~?= False
       ]

-- Exercise 2: Make a list of lists square.

-- Use `min` (you only need one use of it), but not `minimum`.
-- Remember, you should not use any functions from previous exercises.
-- You may use the functions returned in question 2b.

squarify :: [[a]] -> [[a]]
squarify = error "unimplemented"
  where
    dim :: Int
    dim = error "unimplemented"

{-
  type you searched for (type of a function that returns the first `n` elements
  of a list, for some number `n`):
  FILL IN HERE

  name two functions with this type returned on Hoogle:
  FILL IN HERE
-}

exercise2 :: Test
exercise2 =
  "squarify"
    ~: [ squarify jagged ~?= [[1, 2], [5, 6]],
         squarify perfect ~?= perfect,
         squarify [[], [1, 2], [3]] ~?= [],
         squarify [[1, 2, 3], [4, 5, 6]] ~?= [[1, 2], [4, 5]]
       ]

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
        [ exercise7,
          exercise1a,
          exercise1b,
          exercise2,
          check
        ]
  return ()

-- Examples from the instructions, for use in the tests: --

jagged :: [[Int]]
jagged =
  [ [1, 2, 3, 4],
    [5, 6],
    [7, 8, 9]
  ]

perfect :: [[Int]]
perfect =
  [ [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]