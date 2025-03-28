module Exercises where

import Test.HUnit

{- Read [instructions.md] first. -}

-- Exercise 1

q1 :: Bool
q1 = error "unimplemented"

q2 :: Bool
q2 = error "unimplemented"

q3 :: Bool
q3 = error "unimplemented"

-- Exercise 2

lengthFile :: FilePath -> IO Int
lengthFile = error "unimplemented"

exercise2a :: Test
exercise2a =
  "lengthFile"
    ~: [ testLength "two/input1.txt" 20,
         testLength "two/input2.txt" 0,
         testLength "two/input3.txt" 11
       ]

concatFiles :: [FilePath] -> FilePath -> IO ()
concatFiles = error "unimplemented"

exercise2b :: Test
exercise2b =
  "concatFiles"
    ~: [ testConcat
           ["two/input1.txt", "two/input2.txt", "two/input3.txt"]
           "two/result.txt"
           "two/expected.txt"
       ]

-- Exercise 3

putStr' :: String -> IO ()
putStr' = error "unimplemented"

putStr'' :: String -> IO ()
putStr'' = error "unimplemented"

putStrLn' :: String -> IO ()
putStrLn' = error "unimplemented"

print' :: (Show a) => a -> IO ()
print' = error "unimplemented"

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

-- helper code for testing

testLength :: FilePath -> Int -> Assertion
testLength input n = do
  n' <- lengthFile input
  assertEqual "" n n'

testConcat :: [FilePath] -> FilePath -> FilePath -> Assertion
testConcat inputs output expected = do
  oute <- readFile expected
  concatFiles inputs output
  outr <- readFile output
  assertEqual "" oute outr