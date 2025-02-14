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

testAny :: Test
testAny =
  "any"
    ~: [ any even [1, 7, 3] ~?= False,
         any (> 4) [] ~?= False,
         any (/= 'c') "cccac" ~?= True
       ]

bigEnough :: Int -> [Int] -> [Int]
bigEnough n = filter (\x -> abs x >= n)

testBigEnough :: Test
testBigEnough =
  "bigEnough"
    ~: [ bigEnough 0 [0, -1, 5] ~?= [0, -1, 5],
         bigEnough 100 [-50, 1, 6] ~?= [],
         bigEnough 1 [] ~?= []
       ]

-- Exercise 2:

concat :: [[a]] -> [a]
concat = error "unimplemented"

testConcat :: Test
testConcat =
  "concat"
    ~: [ concat [] ~?= ([] :: [()]),
         concat [[1]] ~?= [1],
         concat [[1, 2], [], [3], [4, 5], []] ~?= [1, 2, 3, 4, 5]
       ]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = error "unimplemented"

testConcatMap :: Test
testConcatMap =
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

testFunc1 :: Test
testFunc1 = error "unimplemented"

testFunc2 :: Test
testFunc2 = error "unimplemented"

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
        [ testAny,
          testBigEnough,
          testConcat,
          testConcatMap,
          testFunc1,
          testFunc2,
          check
        ]
  return ()
