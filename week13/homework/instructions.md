# Homework 9: Property-Based Testing

**Due**: Wednesday, April 16 at 11:59 p.m.

## Background

In this homework we will use binary-search trees (BSTs). Recall that in a BST,
at any node with  value `x`, the left subtree has values strictly less than `x`,
and the right subtree has values strictly greater than `x`. For example,

```Haskell
Branch (Branch Leaf 3 Leaf) 4 (Branch Leaf 5 Leaf)
```

is a valid BST. On the other hand,

```Haskell
Branch (Branch Leaf 4 Leaf) 3 (Branch Leaf 5 Leaf)
```

and

```Haskell
Branch (Branch Leaf 4 Leaf) 4 (Branch Leaf 4 Leaf)
```

are invalid. BSTs are _not_ required to be balanced, only ordered with all
distinct elements.

The implementations of several useful functions can be found in `BST.hs`.

## Exercise 1

In this exercise, we will implement a generator for BSTs. Since
these trees are a recursive data type, the generator will also need to
recursively generate the subtrees. How do we enforce that the left subtree
has smaller values than the current node and the right subtree greater values?

One approach is to maintain lower and upper bounds, where `genTree lower upper`
generates BSTs with values strictly between `lower` and `upper`. Fill in the
unimplemented parts by considering the following:

1. When `lower > upper`, we must generate a `Leaf`.
2. Otherwise, we can either make a `Leaf` or a `Branch`. Use the `frequency`
   combinator to assign weights to each case.
   Note: there are no specific weights you are required to choose, but keep in
   mind the following:

   a. `Branch` should have a high enough weight that you can observe moderately
      complex trees getting generated and that most generated trees are not
      just `Leaf`. You can use `verboseCheck` to observe generated test cases.

   b. In general, when assigning weights to a base case and a recursive case,
      we run the risk of stack overflow errors if we make the frequency of the
      recursive case too high relative to that of the base case. In this
      example, we do not have that problem because a side effect of our
      strategy with `lower` and `upper` is that it limits the number of
      recursive calls. That said, larger test cases can be slower, so if you
      have a computer with limited memory and QuickCheck is running very
      slowly, try lowering the frequency of the recursive case.

4. In the `Branch` case, first generate a value `x` between `lower` and
   `upper`. Find a combinator from `QuickCheck` (you can search it on Hoogle)
   to help with this!
5. Then, make recursive calls to `genTree` to generate the left and right
   subtrees, taking care to update the bounds appropriately.

If your generator is correct, then `quickCheck prop_arbitraryValid` should pass.

## Exercise 2

The function `find :: Int -> Tree -> Bool` determines whether or not a value is
in a tree. How can we write a property specifying `find`?

Conceptually, we want `find x t` to return `True` if `x` is in `t` and `False`
if not. But how do we know when writing our property whether `x` is in `t`?
To determine that for an arbitrary tree, we would need a function like `find`!

This problem demonstrates that we often do not want to test functions in
isolation. Instead, we want to test how they _interact_ with other functions.
In this case, `BST.hs` also contains `insert`, which inserts a value into a
tree, and `delete`, which deletes a value from a tree. We will test that `find`
behaves appropriately in concert with those.

Fill in these two properties:

1. `prop_findPostPresent` should check that we can find a value we inserted.
2. `prop_findPostAbsent` should check that we cannot find a value we deleted.

Running `quickCheck` on both properties should pass.

## Exercise 3
Of course, many trivial properties would also pass here, so make sure that your
properties actually match the above specification. To do this, switch out
`findGood` in the definition of `find` for `findBad1`, `findBad2`, and
`findBad3` in turn, testing `prop_findPostPresent` and `prop_findPostAbsent`
with. Record the results in the `Bool`s provided, where `True` indicates
a passing test. (At least one of the properties should fail for each bad
definition.)

## Submission

Submit only `Exercises.hs` to Gradescope, not `BST.hs`.
