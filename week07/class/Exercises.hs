{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

------

{- Exercise : Write an Eq instance for Tree, where two Trees are equal iff
  they have precisely the same elements at precisely the same positions.
  Note: `a` will need to be an Eq instance also. -}

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

-- FILL IN HERE

{- Exercise:
  We might want a different notion of equality -- for example, if we are using
  Tree to implement multisets, we may only care that they have the same
  elements with the same multiplicity in *some* order. Comment out your above
  instance declaration and write another one with this notion of equality.
  Hint: you may convert the tree to another type, and you may require that
  `a` is an instance of `Ord`. -}

-- FILL IN HERE

{- Comment out the second definition and use the first for the rest of the
   exercises. -}

{- Exercise:
 `Implement a function `count`, where `counts x t` counts the instances of
  `x` in a tree `t`. As always, be sure to write the type annotation. -}

-- FILL IN HERE

------

{- Exercise:
  Some data structures have a notion of reversibility. For such structures, we
  might want to reverse them, check if two structures are mirrors/reverses of
  each other, and check if two structures are the same up to reversing.

  Create a type class, Mirror, with these three functions, `mirror`,
  `areMirrors`, and `eqOrMirrors`.
  The second can (and should) have default definitions in terms of the first.

  Hint: You will need to make this type class dependent on another type class.

  Note: in real-world code, programmers make new typeclasses infrequently,
  because the standard library already provides many of the typeclasses a
  Haskell programmer might want. We will see a few more in the coming weeks. -}

-- FILL IN HERE

{- Exercise:
  Write a Mirror instance for Tree, where the mirror is just the original
  tree with the left and right branches swapped at every node. -}

-- FILL IN HERE

{- Exercise:
  Write a Show instance for Tree, showing only the data, not the constructors,
  and with structure indicated via parentheses. For example,
  show (Branch (Branch Leaf 1 Leaf) 2 Leaf) = "(( 1 ) 2 )"
  show Leaf = ""
  -}

-- FILL IN HERE

{- Exercise:
   If you reach this point before class ends, and you have not already done so,
   please write at least two test cases for each function. ("Test case" here
   means one input, not necessarily one instance of `Test`.) -}