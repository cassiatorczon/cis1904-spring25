module Exercises where

import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

-- Streams

data Stream a = Cons a (Stream a)

-- Output the first n elements of a stream as a list
streamTake :: Int -> Stream a -> [a]
streamTake = error "unimplemented"

-- streamGenerate f a should return a, f a, f (f a), ...

streamGenerate :: (a -> a) -> a -> Stream a
streamGenerate = error "unimplemented"

testStreamGenerate :: Test
testStreamGenerate =
  "streamGenerate"
    ~: [ "collatz"
           ~: (streamTake 4 . streamGenerate collatz) 7
           ~?= [7, 22, 11, 34],
         "powers2"
           ~: (streamTake 5 . streamGenerate (* 2)) 1
           ~?= [1, 2, 4, 8, 16]
       ]

-- Using streamGenerate, construct the sequence of all factorials

facts :: Stream Int
facts = error "unimplemented"

testFacts :: Test
testFacts =
  "facts"
    ~: streamTake 5 facts
    ~?= [1, 1, 2, 6, 24]

-- and of all nats
nats :: Stream Integer
nats = error "unimplemented"

testNats :: Test
testNats =
  "nats"
    ~: streamTake
      15
      nats
    ~?= [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

{- Convert a stream to an infinite list. -}
streamToList :: Stream a -> [a]
streamToList = error "unimplemented"

testStreamToList :: Test
testStreamToList =
  "streamToList"
    ~: [ "facts" ~: (take 3 . streamToList) facts ~?= [1, 1, 2],
         "nats" ~: (take 3 . streamToList) nats ~?= [1, 2, 3]
       ]

main = do
  _ <-
    runTestTT $
      TestList
        [ testStreamToList,
          testStreamGenerate,
          testFacts,
          testNats
        ]
  return ()

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
collatz n = 3 * n + 1
