# Algebraic Data Types

## Enumeration types

Like many programming languages, Haskell allows programmers to create
their own _enumeration types_. Here's a simple example:

```Haskell
data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)
```

This declares a new type called `Thing` with five _data constructors_
`Shoe`, `Ship`, etc. which are the (only) values of type `Thing`.

(The `deriving Show` tells GHC to automatically generate code to convert
`Thing`s to `String`s, which allows GHCi to print `Thing`s.)

```Haskell
shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, King, Cabbage, King]
```

We can write functions on `Thing`s by pattern matching.

```Haskell
isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False
```

Recalling how function clauses are tried in order from top to bottom,
we could also make the definition of `isSmall` a bit shorter:

```Haskell
isSmall' :: Thing -> Bool
isSmall' Ship = False
isSmall' King = False
isSmall' _ = True
```

## Beyond enumerations

Enumerations are actually only a special case of Haskell's more general
_algebraic data types_. As a first example of a data type which is not just an enumeration:

```Haskell
data FailableDouble
  = Failure
  | OK Double
  deriving (Show)
```

This says that the `FailableDouble` type has two data constructors.
The first one, `Failure`, takes no arguments, so `Failure` by itself
is a value of type `FailableDouble`. The second one, `OK`, takes an
argument of type `Double`. For example, `OK 3.4` is a value of
type `FailableDouble`.

```Haskell
exD1 :: FailableDouble
exD1 = Failure

exD2 :: FailableDouble
exD2 = OK 3.4
```

Thought exercise: what is the type of `OK`?

```Haskell
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)
```

Time for more pattern-matching! Notice how in the `OK` case we can give a name
to the `Double` that comes along with it.

```Haskell
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d
```

Data constructors can have more than one argument.

```Haskell
-- Store a person's name, age, and favorite Thing.
data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a
```

The type constructor and data constructor are both named
`Person`, but they inhabit different namespaces and are different
things. This is a common idiom for one-constructor data types.

## Algebraic data types in general

In general, an algebraic data type has one or more data constructors,
and each data constructor can have zero or more arguments.

```Haskell
data AlgDataType
  = Constr1 Type11 Type12
  | Constr2 Type21
  | Constr3 Type31 Type32 Type33
  | Constr4
```

This specifies that a value of type `AlgDataType` can be constructed
in one of four ways: using `Constr1`, `Constr2`, `Constr3`, or
`Constr4`. Depending on the constructor used, an `AlgDataType` value
may contain some other values. For example, if it was constructed
using `Constr1`, then it comes along with two values, one of type
`Type11` and one of type `Type12`.

One final note: type and data constructor names must always start with
a capital letter; variables (including names of functions) must always
start with a lowercase letter. Otherwise, Haskell parsers would have
quite a difficult job figuring out which names represent variables and
which represent constructors.

## Pattern matching

We've seen pattern matching in a few specific cases, but let's see how
it works in general. Fundamentally, pattern matching is
about taking apart a value by _finding out which constructor_ it was
built with. This information forms the basis for deciding what to do.

For example, to decide what to do with a value of type `AlgDataType`
(the made-up type defined in the previous section), we could write
something like

```Haskell
foo (Constr1 a b)   = ...
foo (Constr2 a)     = ...
foo (Constr3 a b c) = ...
foo Constr4         = ...
```

This is the main idea behind patterns, but there are a few more things
to note.

1. An underscore `_` can be used as a "wildcard pattern" that
   matches anything.

2. A pattern of the form `x@pat` can be used to match a value against
   the pattern `pat` _and_ give the name `x` to the entire value
   being matched. For example:

    ```Haskell
    displayPerson :: Person -> String
    displayPerson p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n
    ```

3. Patterns can be _nested_. For example:

    ```Haskell
    lovesCabbage :: Person -> Bool
    lovesCabbage (Person _ _ Cabbage) = True
    lovesCabbage _ = False
    ```

    That is, we nested the pattern `Cabbage` inside the pattern for `Person`.

4. Literal values like `2` or `'c'` can be used in pattern matching
   as if they were constructors with no arguments. For example:

    ```Haskell
    isSeventeen :: Person -> Bool
    isSeventeen (Person _ 17 _) = True
    isSeventeen _ = False
    ```

## Case expressions

The fundamental construct for doing pattern-matching in Haskell is the
`case` expression. In general, a `case` expression looks like

```Haskell
case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...
```

When evaluated, the expression `exp` is matched against each of the
patterns `pat1`, `pat2`, ... in turn. The first matching pattern is
chosen, and the entire `case` expression evaluates to the expression
corresponding to the matching pattern. For example,

```Haskell
exCase :: Int
exCase = case "Hello" of
  [] -> 3
  ('H' : s) -> length s
  _ -> 7
```

evaluates to `4`, since the second pattern is the first one that matches.

In fact, the pattern matching syntax we use in functions is just syntactic sugar
for a `case` expression. The definition of `failureToZero` given previously can
equivalently be written as

```Haskell
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d -> d
```

## Recursive data types

Data types can be _recursive_, that is, defined in terms of
themselves. In fact, we have already seen a recursive type: lists.
A list is either empty, or a single element followed by a
remaining list. We could define our own list type like so:

```Haskell
data IntList
  = Empty
  | Cons Int IntList
```

Haskell's own built-in lists are quite similar, though they use
special syntax and work for elements other than just integers — more on this next week.

We often use recursive functions to process recursive data types:

```Haskell
intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x xs) = x * intListProd xs
```

As another simple example, we can define a type of binary trees with
an `Int` value stored at each internal node:

```Haskell
data Tree
  = Leaf
  | Node Tree Int Tree
  deriving (Show)

tree :: Tree
tree = Node Leaf 1 (Node Leaf 2 Leaf)
```
