# Higher-Order Patterns

## Function composition

Recall that in Haskell, functions are _first-class_, i.e., they can be passed
around as values. A function that takes in another function as an argument or
returns another function is called a _higher-order_ function. We have already
seen functions, such as `map`, `filter`, and `fold`, that take other functions
as arguments. Consider the following example that also outputs a function.
(Recall that the `\x -> ` syntax is used for anonymous functions, like
`fun x =>` in OCaml.)

```Haskell
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
```

(In the standard library, we instead use the symbol `(.)` for function
composition. That is, if `f` and `g` are functions, then `f . g` is the
function which first does `g` and then `f`.)

Function composition can help programmers write concise, elegant code. It fits
well in a “wholemeal” programming style, where we think about composing
together successive high-level transformations of a data structure.

Consider the following example:

```Haskell
myTest :: [Int] -> Bool
myTest xs = even (length (filter (== 0) xs))
```

We can rewrite this as:

```Haskell
myTest :: [Int] -> Bool
myTest = even . length . (filter (== 0))
```

This version makes it clearer what is really going on: `myTest` is just a
“pipeline” composed of three smaller functions.

## Partial application

Consider this example:

```Haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

The type of `f` might seem strange. Why all the arrows, instead of something
like `Int Int -> Int`? The answer is that all functions in Haskell actually
only take one argument!

In the above example, `f` takes one argument, of type `Int`, and outputs a
function of type `Int -> Int`. That function then takes one argument and
finally returns an Int. In fact, we can equivalently write `f`’s type like
this:

```Haskell
f :: Int -> (Int -> Int)
```

That is, `->` is _right-associative_, so `W -> X -> Y -> Z` is equivalent to
`W -> (X -> (Y -> Z))` for any `W`, `X`, `Y`, `Z`. We can always add or remove
parentheses around the rightmost top-level arrow in a type.

Function application, in turn, is left-associative. That is, `f 3 4` is really
shorthand for `(f 3) 4`. This is consistent with what we said previously: we
apply `f` to an argument `3`, which returns a function that takes an `Int` and
adds 6 to it.

“Multi-argument” anonymous functions such as `\x y z -> ...` are really just
syntactic sugar for a series of anonymous functions, e.g.,
`\x -> (\y -> (\z -> ...))`. (For convenience, we will continue to refer to
these functions that return functions as multi-argument functions.)

With this knowledge of associativity, we can rewrite the type of our `compose`
function:

```Haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```

This idea of representing multi-argument functions as one-argument functions
returning functions is known as _currying_, named for the British mathematician
and logician Haskell Curry, though it was previously proposed by Moses
Schönfinkel. You may see _currying_ used to refer to the process of converting
a function that takes a single pair as input, e.g.,

```Haskell
f :: (Int, Char) -> Bool
f (x,y) = ...
```

to one that takes the elements of the pair as separate arguments, i.e.,

```Haskell
f :: Int -> Char -> Bool
f x y = ...
```

These two styles are mathematically equivalent, and the reverse conversion is
called _uncurrying_.

The fact that functions in Haskell are curried facilitates
_partial application_, where we supply only some of the arguments to a
multi-argument function so we can use the resulting function.
For example, we can partially apply `filter` to write

```Haskell
greaterThan100 :: [Int] -> [Int]
greaterThan100 = filter (> 100)
```

Here, we want a function from `[Int]` to `[Int]`, so we only supply the first
argument to `filter`.

In general, it is hard in Haskell to partially apply a function to any argument
other than the first. The one exception is infix operators. As we’ve seen,
these can be partially applied to either of their two arguments using an
_operator section_, such as `(> 100)` in the above example.

This means that when writing a multi-argument function, it can make a
difference which order you choose to put the arguments in! Think about which
argument you are most likely to want to partially apply the function to. For
example, with `filter`, we often want to filter several lists using the
same criterion, and we rarely want to filter the same list using multiple
criteria, so it makes sense for its function argument to come first.

## Another example

Let's tie some of the things we've learned together.
Suppose we have a list of rosters for courses, and we want to find
the size of the smallest nonempty course.

```Haskell
minLenNE :: [[String]] -> Int
minLenNE [] = 0
minLenNE (x : xs)
  | x == []   = minLenNE xs
  | otherwise = min (length x) (minLenNE xs)
```

This is not good Haskell style. The problem is that it is a) doing too much at
once, making it less _modular_, and b) working at too low a level.
Instead of thinking about what we want
to do with each element, we want to think about making transformations to the
entire input at once, using the recursion patterns that we have.
Here’s a much more idiomatic implementation:

```Haskell
minLenNE :: [Int] -> Int
minLenNE = minimum . map length . filter (/= [])
```

This defines `minLenLE` as a “pipeline” of three functions:
first, we filter out all empty list elements;
next, we get the length of each element in the resulting list;
finally, we sum the results.

Observe that `map` and `filter` have been partially applied, and we use `(.)`
to compose the functions together.
