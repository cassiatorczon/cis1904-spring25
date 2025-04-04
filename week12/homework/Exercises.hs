{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Exercises where

import System.IO.Silently (capture_)
import Test.HUnit

-- Exercise 1:

fish1 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish1 = error "unimplemented"

fish2 :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
fish2 = error "unimplemented"

join1 :: (Monad m) => m (m a) -> m a
join1 = error "unimplemented"

join2 :: (Monad m) => m (m a) -> m a
join2 = error "unimplemented"

-- Exercise 2:

data Logger a = Logger String a
  deriving (Eq, Show)

square :: Double -> Logger Double
square n = Logger ("squared " ++ show n) (n * n)

squareTwice :: Double -> Logger Double
squareTwice n =
  let Logger log1 n2 = square n
      Logger log2 n4 = square n2
   in Logger (log1 ++ "\n" ++ log2) n4

-- Exercise 2a:

instance Monad Logger where
  return :: a -> Logger a
  return = error "unimplemented"

  (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  (Logger log x) >>= k = error "unimplemented"

squareTwiceM :: Double -> Logger Double
squareTwiceM n = square n >>= square

exercise2a :: Test
exercise2a =
  "2a"
    ~: [squareTwiceM 17 ~?= squareTwice 17]

-- Exercise 2b:

add :: Double -> Double -> Logger Double
add m n = Logger ("added " ++ show m ++ " and " ++ show n) (m + n)

root :: Double -> Logger Double
root n = Logger ("took the square root of " ++ show n) (sqrt n)

pythagorean :: Double -> Double -> Logger Double
pythagorean x y =
  let Logger log1 x2 = square x
      Logger log2 y2 = square y
      Logger log3 z2 = add x2 y2
      Logger log4 z = root z2
   in Logger (log1 ++ "\n" ++ log2 ++ "\n" ++ log3 ++ "\n" ++ log4) z

pythagoreanM :: Double -> Double -> Logger Double
pythagoreanM = error "unimplemented"

exercise2b :: Test
exercise2b =
  "2b"
    ~: [pythagoreanM 3 4 ~?= pythagorean 3 4]

-- Exercise 2c:

when :: (Monad m) => Bool -> m () -> m ()
when = error "unimplemented"

printLogger :: (Show a) => Bool -> Logger a -> IO ()
printLogger verbose (Logger log result) = do
  error "unimplemented"
  print result

exercise2c :: Test
exercise2c =
  "printLogger"
    ~: [ testPrint
           (when True (print 3))
           "3\n",
         testPrint
           (when False (print 3))
           "",
         testPrint
           (printLogger True (square 3.0))
           "squared 3.0\n9.0\n",
         testPrint
           (printLogger False (square 3.0))
           "9.0\n"
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
        [ exercise2a,
          exercise2b,
          exercise2c,
          check
        ]
  return ()

{-
Note that Haskell enforces that in order to for a type constructor to be a
Monad, it also has to be a Functor and an Applicative. To allow us to focus on
monads, we leave these unimplemented in this homework. Please don't modify the
two lines below.
-}

instance Functor Logger

instance Applicative Logger

-- helper code for testing

testPrint :: IO () -> String -> IO ()
testPrint io expected = do
  out <- capture_ io
  assertEqual "" out expected