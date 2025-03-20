-- Uncomment this if you're having trouble with type signatures in instances.
-- {-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Exercises where

import Test.HUnit

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

{-
Exercise:
Write a Functor instance for Tree.
Then, write at least two distinct test cases for fmap.
-}

{-
Exercise:
Use fmap to implement a function `plusThree` that adds three to every
element in the tree. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become Branch (Branch Leaf 4 Leaf) 5 (Branch Leaf 6 Leaf)

Write at least two test cases. The two cases should NOT have the same type
for the elements of the tree.
-}

{-
Exercise:
Write a Foldable instance for Tree.

Note that there is an implicit choice of prefix vs. infix order here.
We will consider the tree in infix order, i.e., a node comes "after" its
left child and "before" its right child.

Write at least two distinct test cases for foldr. Make sure at least one of
your test cases effectively demonstrates that you are folding in the expected
order (and not, e.g., doing a foldl instead).
-}

{-
Exercise:
Use foldr to implement a function `flatten` that flattens a
tree into a list. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become [1, 2, 3]

Write at least two distinct test cases for flatten.
-}

data Pair a b = MkPair a b

{-
Exercise:
Write the kind of each type or type constructor here.
Then check your answers in ghci using `:k`.
1. Pair
2. Pair Int Bool
3. Pair Int
4. Int -> Int
5. Pair (Int -> Int) (Bool -> Int)
6. Pair (Maybe Int)
-}

{-
Exercise:
Part 1: Uncomment the below code and make sure you understand the error message
that occurs. Then comment it out again.

Part 2: In ghci, check the kind of `[Maybe]`. Make sure you understand the
error message that occurs.
-}

-- instance Functor Int where
--   fmap :: Int -> Int
--   fmap = id

{-
Exercise (Challenge):
Write instances of Functor and Foldable for `Pair a`, where `a` is a type
variable.

Hint:
Remember that we said a container here has values of all one type. `Pair a` can
be seen as a container type with values of some type `b`.

Note: this is a challenge exercise. Do not worry if you don't finish it.
-}