# Homework 4: Higher-Order Patterns

**Due**: Wednesday, Feb. 19 at 11:59 p.m.

## Exercise 0

In this exercise and the next, we will practice refactoring code to more closely
obey the idiom that functions should not unnecessarily refer to their arguments
— instead, we can use and manipulate the functions directly.

For each function, you should take care that you are not changing its
functionality, so first carefully read the definition
and make sure you understand it.

### or

In this exercise, you will make two changes to the definition of `or`.

First, notice that we are giving a name to the argument `xs` only to pass it in
directly to the `foldr`. "Eta reduce" by removing `xs` from both sides.

Second, notice that constructing an anonymous function to pass into `foldr` is
actually unnecessary. Rewrite the first argument to `foldr` to be more direct.

### or2

After making these changes to `or` manually, remove the two "HLINT ignore"
lines. You should not see any warnings under your modified `or`, but you
should see warnings appear under the yet unmodified `or2`.

Hover over the warnings, scroll until you see "Quick fix" and then "Apply hint"
for each warning you get.
You should end up with the same code as `or`.

For the rest of the assignment, you can use `hlint` to help you make these two
kinds of changes — but you should make sure you understand why the changes work.

## Exercise 1

Next, we will practice using the composition operator. Note that `hlint` is not
usually able to automatically propose this type of refactoring.

Modify the definition of `any` so that it uses `.` and no longer needs to refer
to the argument `xs`. (It can still refer to the argument `f`).

Modify the definition of `bigEnough` by rewriting `(\x -> abs x >= n)` as the
composition of two functions, so that it no longer needs to refer to an
argument `x`. (It can still refer to `n`.) Recall the _operator sections_ we
discussed in class.

## Exercise 2

Reimplement `concat` and `concatMap` from the list library. `concat` should
concatenate the elements in a list of lists. `concatMap` should map a function
over the elements of a list and concatenate the results. (If you are
unfamiliar with concatenation, look at the test cases as examples.)

You may use `concat` in your definition of `concatMap`.
What's the shortest solution you can come up with, not counting whitespace,
using the techniques we've learned?

## Exercise 3

Reimplement `func` as `func'` in more idiomatic Haskell style. Use wholemeal
programming practices, breaking the function into a pipeline of incremental
transformations.

You may want to use the library function `sum`, which sums together a list of
numbers. You may also want to use some subset of `map`, `filter`, and `fold`.

Make sure that your new function still does the same thing! Please fill in
2 tests for this function. (They should be distinct, but they do not need
to be comprehensive. You may use `func` in your test for comparison. )

## Grading

This homework is largely manually graded due to the nature of
the exercises. If you would like to check your answers, we can provide feedback
in office hours, and you can optionally choose to revise and resubmit.
(The usual deadline still applies to resubmissions.)

Note: the honesty policy prohibits looking at another student's code or allowing
another student to look at yours. It also prohibits sharing the feedback from
office hours with another student. Please come directly to office hours if you
would like feedback. If you post on Ed about your code, please make sure it is
a private post.