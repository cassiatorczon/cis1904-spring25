# Haskell Basics, Part 2

Next, we give a tour of some Haskell features.

## Declarations and variables

```Haskell
x :: Int
x = 3
```

The above code declares a variable `x` with type `Int` (`::` is
pronounced "has type") and declares the value of `x` to be `3`.

Remember how we said that variables are immutable? Suppose we later
also tried to add the line

```Haskell
x = 4
```

This will lead to an error about `Multiple declarations of x`.

In Haskell, `=` does not denote "assignment" like it does in many other languages.
Instead, `=` denotes "definition," like it does in mathematics. That is, `x = 3`
should not be read as "assign `3` to `x`", but as "`x` is _defined to be_ `3`".

## Comments

```Haskell
-- Single line comments are preceded by two hyphens.
{- And multi-line comments are enclosed
   in these special braces. -}
```

## Basic Types

Common types include integers,

```Haskell
i :: Int
i = 42
```

floating-point numbers,

```Haskell
d :: Double
d = 3.14
```

booleans,

```Haskell
b :: Bool
b = False
```

characters,

```Haskell
c :: Char
c = 'x'
```

and strings.

```Haskell
s :: String
s = "Hello, Haskell!"
```

## GHCi

GHCi is an interactive Haskell REPL (Read-Eval-Print-Loop). In a terminal,
start GHCi with

```
> ghci
```

We can evaluate expressions,

```
Prelude> 3 + 4
```

ask for the type of an expression,

```
Prelude> b = False
Prelude> :t b
```

and more. We will also use GHCi in the context of loading
(and reloading) files.

## Arithmetic

The usual arithmetic operations are available in Haskell.

```Haskell
j :: Int
j = 3 + (-2)

k :: Int
k = 19 `mod` 3

e :: Double
e = 8.7 / 3.1
```

And so on.

Note how \`backticks\` make a function name into an infix
operator. Note also that negative numbers must often be surrounded by
parentheses, to avoid being parsed as subtraction.

## Boolean logic

Boolean values can be combined with combinators such as `(&&)`
(logical and), `(||)` (logical or), and `not`. For example,

```Haskell
b2 :: Bool
b2 = True && False

b3 :: Bool
b3 = not (False || True)
```

Things can be compared for equality with `(==)` and `(/=)`, or
compared for order using `(<)`, `(>)`, `(<=)`, and `(>=)`.

```Haskell
b4 :: Bool
b4 = 'a' == 'a'

b5 :: Bool
b5 = (16 /= 3) && ('p' < 'q')
```

Haskell also has `if`-expressions: `if b then t else f`.
Idiomatic Haskell does not use `if` expressions very much, often using
pattern matching or guards instead, as we will see shortly.

## Defining basic functions

Consider this function.

```Haskell
-- Compute the sum of the integers from 1 to n.
sumtorial :: Int -> Int
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)
```

Note the syntax for the type of a function: `sumtorial :: Int ->
Int` says that `sumtorial` is a function which takes an `Int`
as input and yields another `Int` as output.

This function is also our first example of _pattern matching_,
which allows us to do case analysis on our function arguments.

Each clause is checked in order from top to bottom, and the first
matching clause is chosen. For example, `sumtorial 0` evaluates to
`0`, since the first clause is matched.

On the other hand, `sumtorial 3` does not match the first clause
(`3` is not `0`), so the second clause is tried. A variable like
`n` matches anything, so the second clause matches and `sumtorial 3`
evaluates to `3 + sumtorial (3 - 1)` (which can then be evaluated further).

Choices can also be made based on arbitrary Boolean expressions using
_guards_.

```Haskell
hailstone :: Int -> Int
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1
```

## Using functions, and multiple arguments

To apply a function to some arguments, just list the arguments after
the function, separated by spaces, like this:

```Haskell
f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex :: Int
ex = f 3 17 8
```

The above example applies the function `f` to the three arguments `3`,
`17`, and `8`.

## Lists

Lists are one of the most fundamental data types in Haskell.

```Haskell
nums :: [Int]
nums = [1, 2, 3, 19]
```

In fact, strings are just lists of characters. That is, `String` is just an
abbreviation for `[Char]`.

```Haskell
-- hello1 and hello2 are exactly the same.

hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"
```

## Constructing lists

We start with the empty list.

```Haskell
emptyList = []
```

Other lists are built up from the empty list using the _cons_
operator, `(:)`. This operator takes an element and a list, and produces a
new list with the element prepended to the front.

```Haskell
l1, l2, l3, l4 :: [Int]
l1 = 1 : []
l2 = 3 : (1 : [])

l3 = 2 : (3 : (4 : []))
l4 = [2, 3, 4] -- shorthand that is equivalent to l3!
```

## Functions on lists

We can write functions on lists using pattern matching.

```Haskell
intListLength :: [Int] -> Int
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs
```

The first clause says that the length of an empty list is 0. The
second clause says that if the input list looks like `(x:xs)`, with
a first element `x` consed onto a remaining list `xs`, then the
length is one more than the length of `xs`.

Since we don't use `x` at all, we could also replace it by an
underscore: `intListLength (_:xs) = 1 + intListLength xs`.

We can also use nested patterns:

```Haskell
sumEveryTwo :: [Int] -> [Int]
sumEveryTwo [] = [] -- Do nothing to the empty list
sumEveryTwo (x:[]) = [x] -- Do nothing to lists with a single element
sumEveryTwo (x:y:zs) = (x + y) : sumEveryTwo zs
```

## Combining functions

We will often want to build more complex functions by combining many simple ones.

For example, what does this function do? What is `f [1, 2, 3]`?

```Haskell
f :: [Int] -> Int
f xs = sumtorial (intListLength (sumEveryTwo xs))
```

## A word about error messages

Actually, six: **Don't be scared of error messages!**

GHC's error messages can be rather long.
However, they usually contain a lot of useful information! Here's an example:

```
Prelude> 'x' ++ "foo"
<interactive>:1:1:
Couldn't match expected type `[Char]' with actual type `Char'
In the first argument of `(++)', namely 'x'
        In the expression: 'x' ++ "foo"
        In an equation for `it': it = 'x' ++ "foo"
```

First we are told "Couldn't match expected type `[Char]` with actual
type `Char`". This means that _something_ was expected to have a list
type, but actually had type `Char`. What something? The next line tells us:
it's the first argument of `(++)` which is at fault, namely,
`'x'`. The last two lines give us a bit more context.

Now we can see the problem: `'x'` has type `Char`, but it is used as
the first argument to `(++)`, which is expecting a `[Char]` instead.

Reading type error messages is an important part of learning Haskell, and we
will get more and more practice with this skill — and the others alluded to
in this reading — in class and on the homework.
