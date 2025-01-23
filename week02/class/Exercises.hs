module Exercises where

{- Part 1:
   Declarations -}

x :: Int
x = 3

-- Error: x = 4

{- Part 2:
   Basic Types -}

i :: Int
i = 3

j :: Int
j = 4

d :: Double
d = 3.14

e :: Double
e = 1.7

{- GHCi:
    > 3 + 4
    > 8.7 / 3.1
    > mod 19 3
    > 19 `mod` 3
    > i + j
    > d + e
-}

-- Error: bad = i + d

ex1 :: Int
ex1 = i + round d

ex2 :: Double
ex2 = fromIntegral i + d

-- Error: bad = j / i

ex3 :: Int
ex3 = div j i

{- Part 3:
   Functions -}

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

{- Exercise:
   sumto n should evaluate to 0 + 1 + ... + n
   example: sumto 5 should be 15 -}

{- Part 4:
   Lists -}

{- Exercise:
   double xs should double every element in xs
   example: double [1, 2, 3] should be [2, 4, 6] -}

{- Exercise:
   swap xs should swap pairs of elements in xs
   example: swap [1, 2, 3, 4] should be [2, 1, 4, 3] -}
