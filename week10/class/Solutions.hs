-- Uncomment this if you're having trouble with type signatures in instances.
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Solutions where

import Test.HUnit (Test, runTestTT, (~:), (~?=))

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)
  deriving (Eq, Show)

{-
Exercise:
Write a Functor instance for Tree.
Then, write at least two distinct test cases for fmap.
-}

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

testFmap :: Test
testFmap =
  "fmap"
    ~: [ "leaf" ~: fmap (const "") Leaf ~?= Leaf,
         "branch"
           ~: fmap (* 6) (Branch (Branch Leaf 1 Leaf) (-2) Leaf)
           ~?= Branch (Branch Leaf 6 Leaf) (-12) Leaf
       ]

{-
Exercise:
Use fmap to implement a function `plusThree` that adds three to every
element in the tree. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become Branch (Branch Leaf 4 Leaf) 5 (Branch Leaf 6 Leaf)

Write at least two test cases. The two cases should NOT have the same type
for the elements of the tree.
-}

plusThree :: (Num a) => Tree a -> Tree a
plusThree = fmap (+ 3)

testPlusThree :: Test
testPlusThree =
  "plusThree"
    ~: [ "leaf" ~: plusThree Leaf ~?= Leaf,
         "branchDouble"
           ~: plusThree (Branch Leaf 4.2 (Branch Leaf (-2.0) Leaf))
           ~?= Branch Leaf 7.2 (Branch Leaf 1.0 Leaf),
         "branchInt"
           ~: plusThree (Branch Leaf 2 (Branch Leaf 0 Leaf))
           ~?= Branch Leaf 5 (Branch Leaf 3 Leaf)
       ]

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

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr comb acc (Branch l x r) = foldr comb (comb x (foldr comb acc r)) l

testFoldr :: Test
testFoldr =
  "foldr"
    ~: [ "leaf" ~: foldr (+) 3 Leaf ~?= 3,
         "branch"
           ~: foldr
             (:)
             "z"
             (Branch (Branch Leaf 'a' Leaf) 'b' (Branch Leaf 'c' Leaf))
           ~?= "abcz"
       ]

{-
Exercise:
Use foldr to implement a function `flatten` that flattens a
tree into a list. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become [1, 2, 3]

Write at least two distinct test cases for flatten.
-}

flatten :: Tree a -> [a]
flatten = foldr (:) []

testFlatten :: Test
testFlatten =
  "flatten"
    ~: [ "leaf" ~: flatten (Leaf :: Tree (Maybe Bool)) ~?= [],
         "branch"
           ~: flatten (Branch (Branch Leaf 0 Leaf) 1 (Branch Leaf 2 Leaf))
           ~?= [0, 1, 2]
       ]

data Pair a b = MkPair a b

{-
Exercise:
Write the kind of each type or type constructor here.
Then check your answers in ghci using `:k`.
1. Pair :: * -> * -> *
2. Pair Int Bool :: *
3. Pair Int :: * -> *
4. Int -> Int :: *
5. Pair (Int -> Int) (Bool -> Int) :: *
6. Pair (Maybe Int) :: * -> *
-}

{-
Exercise:
Part 1: Uncomment the below code and make sure you understand the error message
that occurs. Then comment it out again.
Explanation: Functor is a class of type constructors of kind * -> *, not of
types (things of kind *). For example, [] and Maybe have kind * -> * and can be
functors, but Int has kind * and cannot.

Part 2: In ghci, check the kind of `[Maybe]`. Make sure you understand the
error message that occurs.
[] has kind * -> *, i.e., we can make a list of Int (beccause this has kind *),
but not of Maybe (because this has kind * -> *). That is, Maybe has the wrong
kind to be an argument to []. We would have to apply Maybe, e.g. to Bool, to
get something of kind *.
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

AFTER you have written your instances, you can look up the solution in Hoogle.
(Go to the definition of Functor, look for `Functor ((,) a)` in the list of
instances, and click on `Source`. Do the same for Foldable.)
-}

instance Functor (Pair a) where
  fmap :: (a2 -> b) -> Pair a1 a2 -> Pair a1 b
  fmap f (MkPair x y) = MkPair x (f y)

instance Foldable (Pair a) where
  foldr :: (a2 -> b -> b) -> b -> Pair a1 a2 -> b
  foldr f z (MkPair _ y) = f y z