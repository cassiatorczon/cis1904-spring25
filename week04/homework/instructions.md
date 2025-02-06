# Homework 3: Recursion Patterns

**Due**: Wednesday, Feb. 12 at 11:59 p.m.

In this homework, we will use `map`, `filter`, and `fold` to define functions
that operate on lists of lists. While they share this common theme, **you should
not need to use any code from one exercise in another**. (You will in some cases
reuse code between parts of the same exercise, however.)

Also, except when prompted to in Exercise 2, you should not need to use any
library functions that operate on lists other than `map`, `filter`, `fold`, and
`length`. **Please do not make any modifications to the imports.**

In Haskell, the convention is to name a term of type `[a]` as `xs` and an
element inside (of type `a`) as `x`. When we instead have type `[[a]]`, the
convention is to name the terms as `xss` and the elements inside
(of type `[a]`) as `xs`.

**Constraint: You may _not_ write any recursive functions in this homework.** You
will instead use recursion indirectly, via `map`, `filter`, and `fold`.

## Exercise -7

Implement `remove7` to rid the input of 7s.

## Exercise 1

Let's introduce some terminology.

Given a list of lists `xss`, we say its _height_ is the number of elements (e.g.
rows) in `xss`. For example, the height of
```
[[1, 2, 3, 4],
 [5, 6],
 [7, 8, 9]]
```
is three, since it contains three lists.

Each row `xs` in `xss` has some _width_, which is the number of elements in
`xs`. In the above example, the widths are four, two, and three.

We say `xss` is _square_ if all of the widths are equal to the height. The above
example is not square, but the below is.
```
[[1, 2, 3],
 [4, 5, 6],
 [7, 8, 9]]
```

### Part (a)

First, implement `all`, where `all f xs` determines whether all elements in `xs`
satisfy the boolean predicate `f`. **Constraint**: Use `foldr`.

### Part (b)

Then, implement `square`, which determines whether a list of lists is square.
**Constraint**: Use `all`.

## Exercise 2

In this exercise, we will force a list of lists to be a square. For example,

```
[[1, 2, 3, 4],
 [5, 6],
 [7, 8, 9]]
```

should be `squarify`ed into

```
[[1, 2],
 [5, 6]]
```

### Part (a)

To determine the dimension `dim` of the resulting square, we take the minimum of
the original's widths and height. Implement `dim`, with the help of the `min`
function, where `min m n` returns the lower of numbers `m` and `n`.
**Constraint**: While your answer should use `min`, you should not use `minimum`.

Notice the use of `where`, which allows us to define things locally to be used
in the body of the definition above.

### Part (b)

We'll want to use a library function that returns the first `n` elements of a
list, for some integer `n`. What would be the _type_ of such a function?
(There are two isomorphic answers; either will work here, but one will
show you two extra functions at the end. Ignore these and look at the
first two distinct results.)

Search the type on [Hoogle](https://hoogle.haskell.org/) — make sure you select
`category:Prelude` from the dropdown. If you searched for the right type, you
should see two distinct functions with this same type, one of which will be
useful to us. Fill in the parts that say "FILL IN HERE" in `Exercises.hs`.

### Part (c)

Implement `squarify`, using parts (a) and (b). Squarification should happen by
only keeping the first `dim` elements in each row, and the first `dim` rows.

You should not need to check if a list of lists is square before squarifying it
— your implementation should work regardless.

## Grading

We will pay particular attention in the style points to whether you followed
the constraints in the instructions and whether you made effective use of
`map`, `filter`, and `foldr`.

Also, remember that Haskell allows for partial application of functions.
When defining a function that adds 1 to the input, for example, instead of
writing `addOne x = x + 1`, you can use the more concise `addOne = (+1)`.
We will not grade you on this, and it is not possible in all exercises,
but we encourage you to use the latter style when possible.