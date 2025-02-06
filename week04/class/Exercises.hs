module Exercises where

import Test.HUnit

-- Exercise 1: We will define some functionality for a course roster.

-- define a datatype for course member roles
data Role
  = Student
  | TA
  | Instructor
  deriving (Show, Eq)

-- Store a course member's school ID number and Role
data Member = Member Int Role
  deriving (Show, Eq)

-- Fill in these two functions:
-- One to get a member's ID number, and one to get their role.

idNum :: Member -> Int
idNum = error "unimplemented"

role :: Member -> Role
role = error "unimplemented"

-- Return the ID numbers of all Students in the input list.
-- Your solution should use map and filter once each.
studentIDs :: [Member] -> [Int]
studentIDs = error "unimplemented"

-- studentIDs memberInput should return [0123, 4321].

memberInput :: [Member]
memberInput =
  [ Member 0123 Student,
    Member 4567 TA,
    Member 0000 Instructor,
    Member 4321 Student
  ]

{-
Exercise 2:

Reimplement map and filter using foldr.
Remember, foldr is right-associative:
  foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
-}

map' :: (a -> b) -> [a] -> [b]
map' = error "unimplemented"

filter' :: (a -> Bool) -> [a] -> [a]
filter' = error "unimplemented"

{-
Exercise 3:

Try implementing foldl! To avoid naming conflicts, we will call it foldlE.
Remember, foldl is left-associative:
  foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

Hint: you can do this with two cases, without using foldr or reversing the list.
-}

foldlE :: (b -> a -> b) -> b -> [a] -> b
foldlE = error "unimplemented"

{-
Finally, test your functions!
Start a REPL with `stack ghci` and run `main`.
-}

testStudentIDs :: Test
testStudentIDs =
  "studentIDs"
    ~: [ "empty" ~: studentIDs [] ~?= [],
         "noStudents"
           ~: studentIDs [Member 0123 TA, Member 4321 Instructor]
           ~?= [],
         "oneStudent" ~: studentIDs [Member 0000 Student] ~?= [0000],
         "oneStudentInfix"
           ~: studentIDs
             [Member 5210 Instructor, Member 1600 Student, Member 4819 TA]
           ~?= [1600],
         "ex"
           ~: studentIDs memberInput
           ~?= [0123, 4321]
       ]

testMap' :: Test
testMap' =
  "map'"
    ~: [ "empty" ~: map' (++ "x") [] ~?= [],
         "ex" ~: map' idNum memberInput ~?= map idNum memberInput
       ]

testFilter' :: Test
testFilter' =
  "filter'"
    ~: [ "empty" ~: filter' (0 <) [] ~?= filter (0 <) [],
         "oneTrue" ~: filter' id [True] ~?= filter id [True],
         "allFalse" ~: filter' (/= 'c') "cccc" ~?= filter (/= 'c') "cccc",
         "ex"
           ~: filter' (\x -> Student == role x) memberInput
           ~?= filter (\x -> Student == role x) memberInput
       ]

testFoldlE :: Test
testFoldlE =
  "foldlE"
    ~: [ "empty" ~: foldlE (+) 0 [] ~?= 0,
         "subtraction" ~: foldlE (-) 0 [1, 1, 1] ~?= foldl (-) 0 [1, 1, 1],
         "average"
           ~: foldlE (\x y -> (x + y) / 2) 0 [8, 4, 16]
           ~?= foldl (\x y -> (x + y) / 2) 0 [8, 4, 16],
         "append"
           ~: foldlE (++) "z" ["a", "b", "c"]
           ~?= foldl (++) "z" ["a", "b", "c"],
         "show"
           ~: foldlE (\acc elem -> acc ++ " " ++ show elem) [] [Student, TA, Instructor]
           ~?= foldl (\acc elem -> acc ++ " " ++ show elem) [] [Student, TA, Instructor]
       ]

main :: IO Counts
main = runTestTT (TestList [testStudentIDs, testMap', testFilter', testFoldlE])