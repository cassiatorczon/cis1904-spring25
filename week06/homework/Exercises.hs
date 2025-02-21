{-# LANGUAGE BangPatterns #-}

module Exercises where

import Data.Char (toLower)
import Test.HUnit

-- Exercise 1: Fibonacci numbers

{- Define the Fibonacci numbers, starting with 0. For n < 0, return the
    negation of the |n|th (zero-indexed) Fibonacci number.
    A simple recursive definition is fine. -}

fib :: Integer -> Integer
fib = error "unimplemented"

testFib :: Test
testFib =
  "fib"
    ~: [ "zero" ~: fib 0 ~?= 0,
         "one" ~: fib 1 ~?= 1,
         "six" ~: fib 6 ~?= 8,
         "negFive" ~: fib (-5) ~?= (-5)
       ]

{- Unfortunately, `fib` is very slow. The complexity of calculating the nth
   number is exponential in n. Below we have a tail recursive version. `go` is
   the customary keyword to use in Haskell for this sort of
   internally-defined auxiliary function. `!` is the operator we saw in class
   that forces strict evaluation.
   This definition is just for fun: you do not need to edit it, and you
   should NOT use it in any of the other problems. -}

fib' :: Integer -> Integer
fib' n = go (0, 1) n
  where
    go (!x, !y) !n
      | n > 0 = go (y, x + y) (n - 1)
      | n < 0 = -(go (y, x + y) ((-n) - 1))
      | otherwise = x

{- Define the infinite list of all (nonnegative) Fibonacci numbers, using fib.
    Hint: `[0..100]` denotes the list of all integers from 0 to 10, inclusive.
    `[0..]` denotes the list of all nonnegative integers.
    You should NOT use direct recursion in this problem; combinators such as
    `map`, `filter`, and `fold` are fine. -}

fibs :: [Integer]
fibs = error "unimplemented"

testFibs :: Test
testFibs =
  "fibs"
    ~: [ "ten" ~: take 10 fibs ~?= [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
       ]

{- Unfortunately, `fibs` is also very slow. Laziness means we don't calculate
    elements until we need them, but when we do want them, the calculation
    takes a long time. Define a version where computing the first n elements
    takes O(n) additions.
    Hint:
         fibs = [0, 1, 1, 2, ...]
    tail fibs = [1, 1, 2, 3, ...]

    What happens if we use zipWith to add these two together?
    Fill in the function below.

  Note: normally we discourage use of `tail` because it is partial, but you may
  use it for this exercise. We will discuss better alternatives in the
  exercises below.
-}

fibs' :: [Integer]
fibs' = error "unimplemented"

testFibs' :: Test
testFibs' =
  "fibs'"
    ~: [ "ten" ~: take 10 fibs' ~?= [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
       ]

{- In the example above, we have to either use the partial function `tail`, or
    match on `fibs` and come up with some default behavior in the impossible
    case that `fibs` is the empty list. This is annoying; it would be nice to
    have a type that specified that infinite lists like `fibs` are never empty.
    (Capturing such invariants in the type means that the compiler can check
    them for us!)
    In the finite case, we might use a nonempty list type; here, we use streams
    to also capture within the type the fact that they are infinite. -}

-- Exercise 2: Streams

{- We saw the below definition in class. We implemented some standard
    library functions for it in class; we will write a few more here. -}
data Stream a = Cons a (Stream a)

{- Create a Stream of one value repeated infinitely. Direct recursion is
    fine here. -}
streamRepeat :: a -> Stream a
streamRepeat = error "unimplemented"

testStreamRepeat :: Test
testStreamRepeat =
  "streamRepeat"
    ~: [ "fiveZeros" ~: (streamTake 5 . streamRepeat) 0 ~?= [0, 0, 0, 0, 0],
         "tenCs" ~: (streamTake 10 . streamRepeat) 'C' ~?= "CCCCCCCCCC"
       ]

{- Make a Stream from a list. If the list is finite, all remaining elements
    have the input default value. Remember the style guidelines we practiced
    in the Higher Order unit. How succinct can you make this definition? -}
streamFromList :: a -> [a] -> Stream a
streamFromList = error "unimplemented"

testStreamFromList :: Test
testStreamFromList =
  "streamFromList"
    ~: [ "empty" ~: (streamTake 3 . streamFromList 2) [] ~?= [2, 2, 2],
         "several" ~: (streamTake 3 . streamFromList 1) [2] ~?= [2, 1, 1],
         "all" ~: (streamTake 3 . streamFromList 0) [4, 2, 6] ~?= [4, 2, 6]
       ]

{- map, but for Streams. A directly recursive definition is fine here. -}
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = error "unimplemented"

testStreamMap :: Test
testStreamMap =
  "streamMap"
    ~: [ "fiveThrees"
           ~: (streamTake 5 . streamMap (* 3) . streamRepeat) 1
           ~?= [3, 3, 3, 3, 3],
         "tencs"
           ~: (streamTake 10 . streamMap toLower . streamRepeat) 'C'
           ~?= "cccccccccc"
       ]

{- Finally, reimplement fibs' for Streams. Hint: make a version of tail and
    zipWith for Streams as helper functions. Note that in this case tail is not
    partial! -}
fibs'' :: Stream Integer
fibs'' = error "unimplemented"

testFibs'' :: Test
testFibs'' =
  "fibs''"
    ~: [ "ten" ~: streamTake 10 fibs'' ~?= [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
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
        [ testFib,
          testFibs,
          testFibs',
          testStreamRepeat,
          testStreamFromList,
          testStreamMap,
          testFibs'',
          check
        ]
  return ()

{- Testing infrastructure -}

streamTake :: Int -> Stream a -> [a]
streamTake n (Cons x xs) | n > 0 = x : streamTake (n - 1) xs
streamTake _ _ = []

showStream :: (Show a) => Int -> Stream a -> String
showStream n = show . streamTake n

streamGenerate :: (a -> a) -> a -> Stream a
streamGenerate f x = Cons x (streamGenerate f (f x))

instance (Show a) => Show (Stream a) where
  show = showStream 5

{- Some streams for testing -}

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
collatz n = 3 * n + 1

ackermann :: Integer -> Integer -> Integer
ackermann m n | m < 0 || n < 0 = 0
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz7 :: Stream Integer
collatz7 = streamGenerate collatz 7

ackermann2_1 :: Stream Integer
ackermann2_1 = streamGenerate (ackermann 2) 1

triangle :: Stream Integer
triangle = go 0
  where
    go x = Cons ((x ^ 2 + x) `div` 2) (go (x + 1))

batteries :: Stream String
batteries = streamGenerate ('A' :) "A"

{- Tests for provided functions -}

testFib' :: Test
testFib' =
  "fib'"
    ~: [ "zero" ~: fib' 0 ~?= 0,
         "one" ~: fib' 1 ~?= 1,
         "six" ~: fib' 6 ~?= 8,
         "negFive" ~: fib' (-5) ~?= (-5)
       ]

testCollatz7 :: Test
testCollatz7 =
  "collatz7"
    ~: streamTake 17 collatz7
    ~?= [7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]

testAckermann2_1 :: Test
testAckermann2_1 =
  "ackermann2_1"
    ~: streamTake 10 ackermann2_1
    ~?= [1, 5, 13, 29, 61, 125, 253, 509, 1021, 2045]

testTriangle :: Test
testTriangle =
  "triangle"
    ~: streamTake 5 triangle
    ~?= [0, 1, 3, 6, 10]

testBatteries :: Test
testBatteries =
  "batteries"
    ~: streamTake 5 batteries
    ~?= ["A", "AA", "AAA", "AAAA", "AAAAA"]

testProvided :: IO ()
testProvided = do
  _ <-
    runTestTT $
      TestList
        [ testFib',
          testCollatz7,
          testAckermann2_1,
          testTriangle,
          testBatteries
        ]
  return ()