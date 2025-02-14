module Exercises where

{- Don't change the imports. -}
import Test.HUnit
import Prelude hiding (any, concat, concatMap, or)

{- Read [instructions.md] first. -}

-- Exercise 0:

{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}

or :: [Bool] -> Bool
or xs = foldr (\x acc -> x || acc) False xs

or2 :: [Bool] -> Bool
or2 xs = foldr (\x acc -> x || acc) False xs

-- Exercise 1

any :: (a -> Bool) -> [a] -> Bool
any f xs = or (map f xs)

bigEnough :: Int -> [Int] -> [Int]
bigEnough n = filter (\x -> abs x >= n)

-- Exercise 2:

concat :: [[a]] -> [a]
concat = error "unimplemented"

exercise2a :: Test
exercise2a =
  "concat"
    ~: [ concat [] ~?= ([] :: [()]),
         concat [[1]] ~?= [1],
         concat [[1, 2], [], [3], [4, 5], []] ~?= [1, 2, 3, 4, 5]
       ]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = error "unimplemented"

exercise2b :: Test
exercise2b =
  "concatMap"
    ~: [ concatMap f [] ~?= [],
         concatMap f [1] ~?= [1, 1],
         concatMap f [1, 2, 3] ~?= [1, 1, 2, 2, 3, 3]
       ]
  where
    f :: Int -> [Int]
    f = replicate 2

-- Exercise 3:

func :: [Int] -> Int
func [] = 0
func (x : xs)
  | even x = (x * 3) + func xs
  | otherwise = func xs

func' :: [Int] -> Int
func' = error "unimplemented"

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
        [ exercise2a,
          exercise2b,
          check
        ]
  return ()
