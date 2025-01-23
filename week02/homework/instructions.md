# Homework 1: Haskell Basics

**Due**: Wednesday, Jan 29 at 11:59 p.m.

## Reminder

You should have exactly the `homework` folder open, not a parent folder (like
`week01`) or a specific file (like `Exercises.hs`). Please make sure you are
getting type-checking feedback before you start!

## Exercise 0: Practice with Parentheses

A common source of early syntactic confusion in Haskell is the way the language
uses parentheses — small mistakes can lead to serious parsing issues, which in
turn lead to misleadingly alarming error messages! In this exercise, fix the
syntactic errors in each definition.

## Exercises 1–6: Validating Credit Card Numbers

Have you ever wondered how websites validate your credit card number when you
shop online? They don’t check a massive database of numbers, and they don’t use
magic. In fact, most credit providers rely on a checksum formula called the Luhn
algorithm for distinguishing valid numbers from random collections of digits (or
typing mistakes).

In this problem set, you will implement the algorithm, which follows this
specification:

- Considering the digits of the card number in _reverse order_, double the value
  of every other digit. For example, `9455` becomes `[5, 5, 4, 9]` becomes
  `[5, 10, 4, 18]`.

- Add the digits of the doubled values and the undoubled digits from the
  original number. For example, `[5, 10, 4, 18]` becomes
  `5 + (1 + 0) + 4 + (1 + 8) = 19`.

- Calculate the remainder when the sum is divided by 10. For the above example,
  the remainder would be 9.

- If the result equals 0, then the number is valid.

The progression of the exercises shows a common feature of good Haskell
programming: writing smaller functions that each perform a single task and then
combining these pieces to create more complex functions.

Same as Homework 0, you can execute the tests by running

```
> stack ghci Exercises.hs

Prelude> main
```

When all tests pass, the output will be

```
Cases: 10  Tried: 10  Errors: 0  Failures: 0
```

You can also execute the tests for an individual exercise (e.g. Exercise 1) with

```
Prelude> runTestTT exercise1
```

## Grading

Please see Gradescope for the points breakdown.

In this class, we will have _private_ (but not hidden) tests. That is, the
Gradescope tests are a superset of the ones we have provided here, but you can
submit and see the results (but not contents) of the tests as many times as you
like. The purpose is primarily to prevent hard-coding, not to be onerous.

To receive the style points, you should do your best to write idiomatic Haskell,
such as by effectively using pattern matching, writing type signatures for every
top-level definition, and avoiding unnecessary repetition.

## Source

This assignment is adapted from past offerings of this class, which in turn
adapted it from the first practicum assigned in the University of Utrecht
functional programming course taught by Doaitse Swierstra, 2008-2009.
