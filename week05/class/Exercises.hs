module Exercises where

import Data.Char (toUpper)
import Data.List (sort)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~:),
    (~?=),
  )
import Prelude hiding (and, sum)

{-
In each section, fill in the unimplemented function using the indicated
style guidelines.
-}

-- #1 Use function names directly

-- Instead of this
absAll :: [Int] -> [Int]
absAll xs = map (\x -> abs x) xs

-- Like this
absAll' :: [Int] -> [Int]
absAll' xs = map abs xs

-- Exercise: negate each element of the list (use `not`)
notAll :: [Bool] -> [Bool]
notAll = error "unimplemented"

-- Use infix operators directly too

-- Instead of this
sum :: [Int] -> Int
sum xs = foldr (\x acc -> x + acc) 0 xs

-- Like this
sum' :: [Int] -> Int
sum' xs = foldr (+) 0 xs

-- Exercise: "and" together all elements of a list (use `&&`)
and :: [Bool] -> Bool
and = error "unimplemented"

--------------

-- #2 Leverage function composition

-- Instead of this
revSort :: [Int] -> [Int]
revSort xs = reverse (sort xs)

-- Like this
revSort' :: [Int] -> [Int]
revSort' = reverse . sort

-- Use . when constructing arguments to other functions

-- Instead of this
oddOnly :: [Int] -> [Int]
oddOnly xs = filter (\x -> not (even x)) xs

-- Like this
oddOnly' :: [Int] -> [Int]
oddOnly' xs = filter (not . even) xs

-- Exercise: count the words in each string (use `words` and `length`)
wordCounts :: [String] -> [Int]
wordCounts = error "unimplemented"

-- Use . when combining more than two functions

-- Instead of this
evenOdds :: [Int] -> Bool
evenOdds xs = even (length (oddOnly xs))

-- Like this
evenOdds' :: [Int] -> Bool
evenOdds' = even . length . oddOnly

-- Exercise: count the number of odd numbers in a list and convert the result
-- to a string (use `show` and `oddOnly'`)
showNumOdds :: [Int] -> String
showNumOdds = error "unimplemented"

--------------

-- #3 Leverage partial application.

-- Use partial application to eta reduce
-- (i.e., avoid unnecessarily writing the argument on both sides)

-- Like this (instead of `absAll'`)
absAll'' :: [Int] -> [Int]
absAll'' = map abs

-- Exercise: Implement `and` again, still using `&&`, but eta reduced this time
and' :: [Bool] -> Bool
and' = error "unimplemented"

-- Use partial application via operator sections

-- Instead of this
add3All :: [Int] -> [Int]
add3All = map (\x -> 3 + x)

-- Like this
add3All' :: [Int] -> [Int]
add3All' = map (3 +)

-- Exercise: filter, keeping only elements greater than 100
greaterThan100 :: [Int] -> [Int]
greaterThan100 = error "unimplemented"

--------------

-- Combining all of these

-- Exercise: reimplement these functions from last class using these principles
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' = error "unimplemented"

studentIDs :: [Member] -> [Int]
studentIDs xs = map idNum (filter (\x -> role x == Student) xs)

studentIDs' :: [Member] -> [Int]
studentIDs' = error "unimplemented"

{-- Tests --}
-- notAll, and, wordcounts, shownumodds, and', greaterthan100, studentIDs'
testNotAll :: Test
testNotAll =
  "notAll"
    ~: [ "empty" ~: notAll [] ~?= [],
         "FFT" ~: notAll [False, False, True] ~?= [True, True, False],
         "T" ~: notAll [True] ~?= [False]
       ]

testAnd :: Test
testAnd =
  "and"
    ~: [ "empty" ~: and [] ~?= True,
         "TTTTFT" ~: and [True, True, True, True, False, True] ~?= False,
         "T" ~: and [True] ~?= True
       ]

testWordCounts :: Test
testWordCounts =
  "wordCounts"
    ~: [ "empty" ~: wordCounts [] ~?= [],
         "nonempty" ~: wordCounts ["this has four words", "just three here"] ~?= [4, 3]
       ]

testShowNumOdds :: Test
testShowNumOdds =
  "showNumOdds"
    ~: [ "empty" ~: showNumOdds [] ~?= "0",
         "noOdds" ~: showNumOdds [0, 2, -4] ~?= "0",
         "someOdds" ~: showNumOdds [1, 4, 8, 7, 9] ~?= "3",
         "allOdds" ~: showNumOdds [11] ~?= "1"
       ]

testAnd' :: Test
testAnd' =
  "and'"
    ~: [ "empty" ~: and' [] ~?= True,
         "TTT" ~: and' [True, True, True] ~?= True,
         "FT" ~: and' [False, True] ~?= False
       ]

testGreaterThan100 :: Test
testGreaterThan100 =
  "greaterThan100"
    ~: [ "empty" ~: greaterThan100 [] ~?= [],
         "neg" ~: greaterThan100 [-500, 4, 200] ~?= [200],
         "eq" ~: greaterThan100 [7, 40000, 100] ~?= [40000]
       ]

testMap'' :: Test
testMap'' =
  "map''"
    ~: [ "toUpper" ~: map'' toUpper "abc" ~?= "ABC",
         "empty" ~: map'' sort ([] :: [[Int]]) ~?= [],
         "neg" ~: map'' (* (-1)) [12, -4, 0] ~?= [-12, 4, 0]
       ]

testStudentIDs' :: Test
testStudentIDs' =
  "studentIDs'"
    ~: [ "empty" ~: studentIDs' [] ~?= [],
         "nonempty"
           ~: studentIDs'
             [Member 4000 Student, Member 9000 TA, Member 1001 Student]
           ~?= [4000, 1001]
       ]

main :: IO Counts
main =
  runTestTT
    ( TestList
        [ testNotAll,
          testAnd,
          testWordCounts,
          testShowNumOdds,
          testAnd',
          testGreaterThan100,
          testMap'',
          testStudentIDs'
        ]
    )

{-- Definitions for the studentIDs example --}

data Role
  = Student
  | TA
  | Instructor
  deriving (Show, Eq)

data Member = Member Int Role
  deriving (Show, Eq)

idNum :: Member -> Int
idNum (Member i _) = i

role :: Member -> Role
role (Member _ r) = r