{-# LANGUAGE InstanceSigs #-}

module Solutions where

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

instance (Eq a) => Eq (Tree a) where
  Leaf == Leaf = True
  Branch l1 v1 r1 == Branch l2 v2 r2 = l1 == l2 && v1 == v2 && r1 == r2
  _ == _ = False

testEq :: Test
testEq =
  "eq"
    ~: [ "leaf" ~: (Leaf :: Tree Int) == Leaf ~?= True,
         "branch" ~: Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf
           == Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf ~?= True,
         "mirror" ~: Branch Leaf 2 (Branch (Branch Leaf 0 Leaf) 1 Leaf)
           == Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf ~?= False,
         "wrong" ~: Branch Leaf 1 Leaf == Leaf ~?= False
       ]

{- Exercise:
  We might want a different notion of equality -- for example, if we are using
  Tree to implement multisets, we may only care that they have the same
  elements with the same multiplicity in *some* order. Comment out your above
  instance declaration and write another one with this notion of equality.
  Hint: you may convert the tree to another type, and you may require that
  `a` is an instance of `Ord`. -}

-- instance (Eq a, Ord a) => Eq (Tree a) where
--   t1 == t2 = toSet t1 == toSet t2
--     where
--       toSet Leaf = Set.empty
--       toSet (Branch l v r) =
--         Set.singleton v
--           `Set.union` toSet l
--           `Set.union` toSet r

testEq' :: Test
testEq' =
  "eq'"
    ~: [ "leaf" ~: (Leaf :: Tree Int) == Leaf ~?= True,
         "branch" ~: Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf
           == Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf ~?= True,
         "mirror" ~: Branch Leaf 2 (Branch (Branch Leaf 0 Leaf) 1 Leaf)
           == Branch (Branch Leaf 1 (Branch Leaf 0 Leaf)) 2 Leaf ~?= True,
         "cycle" ~: Branch Leaf 2 (Branch (Branch Leaf 0 Leaf) 1 Leaf)
           == Branch (Branch Leaf 0 (Branch Leaf 2 Leaf)) 1 Leaf ~?= True,
         "wrong" ~: Branch Leaf 1 Leaf == Leaf ~?= False
       ]

{- Comment out the second definition and use the first for the rest of the
   exercises. -}

{- Exercise:
 `Implement a function count, where counts x t counts the instances of
  x in a tree t. As always, be sure to write the type annotation. -}

count :: (Eq a) => a -> Tree a -> Int
count x Leaf = 0
count x (Branch l v r) | x == v = 1 + count x l + count x r
count x (Branch l v r) = count x l + count x r

testCount :: Test
testCount =
  "count"
    ~: [ "leaf" ~: count 1 Leaf ~?= 0,
         "branch" ~: count 2 (Branch Leaf 2 (Branch (Branch Leaf 0 Leaf) 1 Leaf)) ~?= 1,
         "branch2" ~: count 2 (Branch Leaf 2 (Branch (Branch Leaf 2 Leaf) 2 Leaf)) ~?= 3,
         "branch3" ~: count 9 (Branch Leaf 2 (Branch (Branch Leaf 0 Leaf) 1 Leaf)) ~?= 0
       ]

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

class (Eq a) => Mirror a where
  mirror :: a -> a
  areMirrors :: a -> a -> Bool
  eqOrMirrors :: a -> a -> Bool

  areMirrors x y = x == mirror y
  eqOrMirrors x y = x == y || areMirrors x y

{- Exercise:
  Write a Mirror instance for Tree, where the mirror is just the original
  tree with the left and right branches swapped at every node. -}

instance (Eq a) => Mirror (Tree a) where
  mirror Leaf = Leaf
  mirror (Branch l v r) = Branch (mirror r) v (mirror l)

testMirror :: Test
testMirror =
  "mirror"
    ~: [ "leaf" ~: mirror (Leaf :: Tree Char) ~?= Leaf,
         "branch" ~: mirror tree0 ~?= tree1
       ]

testAreMirrors :: Test
testAreMirrors =
  "testAreMirrors"
    ~: [ "leaf" ~: areMirrors (Leaf :: Tree Char) Leaf ~?= True,
         "branch" ~: areMirrors tree1 tree0 ~?= True,
         "wrong1" ~: areMirrors tree1 Leaf ~?= False,
         "wrong2" ~: areMirrors tree0 tree0 ~?= False,
         "sym" ~: areMirrors tree2 tree2 ~?= True
       ]

testEqOrMirrors :: Test
testEqOrMirrors =
  "testEqOrMirrors"
    ~: [ "leaf" ~: eqOrMirrors (Leaf :: Tree Char) Leaf ~?= True,
         "branch" ~: eqOrMirrors tree1 tree0 ~?= True,
         "wrong1" ~: eqOrMirrors tree1 Leaf ~?= False,
         "wrong2" ~: eqOrMirrors tree0 tree2 ~?= False,
         "eq" ~: eqOrMirrors tree0 tree0 ~?= True
       ]

{- Exercise:
  Write a Show instance for Tree, showing only the data, not the constructors,
  and with structure indicated via parentheses. For example,
  show (Branch (Branch Leaf 1 Leaf) 2 Leaf) = "(( 1 ) 2 )"
  show Leaf = ""
  -}

instance (Show a) => Show (Tree a) where
  show Leaf = ""
  show (Branch l v r) = "(" ++ show l ++ " " ++ show v ++ " " ++ show r ++ ")"

testShow :: Test
testShow =
  "show"
    ~: [ "leaf" ~: show (Leaf :: Tree Bool) ~?= "",
         "branch" ~: show tree0 ~?= "((( 4 ) 2 ( 5 )) 1 ( 3 ))"
       ]

{- Exercise:
   If you reach this point before class ends, and you have not already done so,
   please write at least two test cases for each function. ("Test case" here
   means one input, not necessarily one instance of `Test`.) -}

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ testEq,
          testCount,
          testMirror,
          testAreMirrors,
          testEqOrMirrors,
          testShow
        ]

  return ()

main' :: IO ()
main' = do
  _ <-
    runTestTT testEq'

  return ()

-- trees for testing

tree0 :: Tree Integer
tree0 =
  Branch
    ( Branch
        (Branch Leaf 4 Leaf)
        2
        (Branch Leaf 5 Leaf)
    )
    1
    (Branch Leaf 3 Leaf)

tree1 :: Tree Integer
tree1 =
  Branch
    (Branch Leaf 3 Leaf)
    1
    ( Branch
        (Branch Leaf 5 Leaf)
        2
        (Branch Leaf 4 Leaf)
    )

tree2 :: Tree Integer
tree2 =
  Branch
    ( Branch
        (Branch Leaf 5 Leaf)
        2
        (Branch Leaf 4 Leaf)
    )
    1
    ( Branch
        (Branch Leaf 4 Leaf)
        2
        (Branch Leaf 5 Leaf)
    )
