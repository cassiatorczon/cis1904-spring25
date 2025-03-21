# Homework 6: Functor, Foldable

**Due**: Wednesday, March 26 at 11:59 p.m.

## Overview

The purpose of this homework is to practice reading and writing Haskell code at
a high level of abstraction, with heavy generalization over type variables.

Approach these problems by reading the type signature, thinking about the types
of the things (functions, arguments, etc.) that you have available to you, and
then figuring out how to combine them in a way that gets you the desired type.

## Exercise 1

To practice with the `Functor` type class, implement `fconst` and `unzip`.

For this problem, rather than trying to explain what `fconst` and `unzip`
_should_ do (which could actually lead you astray here), we encourage you
to look at the type signatures and think about what they _could_ do.
For each of these types, there is actually only one function of that type!
(Up to equivalence -- technically you could, for example, compose any
function with `id` to make it look different, but we are counting that as
the same.)

Put another way, this means that for this problem, any (safe) implementations
that type check will be correct. This is one of the benefits of Haskell's
strong type system!

(Note: this is not true of the other problems, so be sure to use the provided
tests for those.)

## Exercise 2

Up until now, the various trees we have worked with have been binary trees. An
alternative data structure is the rose tree, where each node instead has a list
of children. e.g.

```Haskell
data RoseTree a = Node a [RoseTree a]
```

Implement a `Functor` instance for `RoseTree`.

Constraint: You may **not** pattern-match on the list of child trees. Instead,
you should use `fmap` (remember, `[]` is a `Functor`, so it has its own
definition of `fmap`).

## Exercise 3

Next, we practice with the `Foldable` type class.
Implement a `Foldable` instance for `RoseTree`.
We will fold in _prefix order_, that is, we think of a node as coming "earlier"
in the tree than its children.

I strongly recommend that you make use of typed holes (via underscores) to
*incrementally* write this function. For example, we should have

```Haskell
foldr f b (Node a ts) = f _ _
```

Figure out what the first argument to `f` should be: What is its type? What do
we have of that type? Then, figure out the second argument: What is its type?
How can we (again, incrementally!) build something of that type?

Constraint: You may **not** pattern-match on the list of child trees. Instead,
you should use the `foldr` for lists.

## Exercise 4

We can further generalize `RoseTree`. Modify `RoseTreeG` to take another type
parameter `t` before the type parameter `a`, so that `NodeG`s contain not a list
of children but a `t` of children.

Copy and paste your `Foldable` instance, and adapt it for `RoseTreeG`. You
should be able to do this in a way where you're only modifying the types (and one
constructor), not the structure of the implementation.

Hint: If you're stuck on syntax, look at the Typeclasses lecture
where we discussed making `Pair a b` an instance of `Show`.

(As an *optional* challenge problem, you can do this for `Functor`, too.)