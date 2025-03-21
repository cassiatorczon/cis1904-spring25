module Exercises where

import Test.HUnit (Test (TestList), runTestTT, (~:), (~?=))

-- Streams

data Stream a = Cons a (Stream a)

{- Output the first n elements of a stream as a list. For negative n, return
    the empty list -}
streamTake :: Int -> Stream a -> [a]
streamTake = error "unimplemented"

{- streamGenerate f x should return the stream containing
    x, f x, f (f x), f (f (f x)), ... -}
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

{- Using streamGenerate, construct the sequence of all natural numbers -}
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
    ~: [ "nats" ~: (take 3 . streamToList) nats ~?= [0, 1, 2]
       ]

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ testStreamToList,
          testStreamGenerate,
          testNats
        ]
  return ()

collatz :: Integer -> Integer
collatz n | even n = n `div` 2
collatz n = 3 * n + 1
