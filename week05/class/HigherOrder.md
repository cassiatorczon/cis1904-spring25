# Higher-Order Patterns

## Function composition

Recall that in Haskell, functions are _first-class_, i.e., they can be passed
around as values. A function that takes in another function as an argument or
returns another function is called a _higher-order_ function. We have already
seen functions, such as `map`, `filter`, and `fold`, that take other functions
as arguments and, if partially applied, return functions.
Consider the following example that also takes in and outputs functions.
(Recall that the `\x -> ` syntax is used for anonymous functions, like
`fun x =>` in OCaml.)

```Haskell
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
```

(In the standard library, we instead use the symbol `(.)` for function
composition. That is, if `f` and `g` are functions, then `f . g` is the
function which first does `g` and then `f`.)

Function composition can help programmers write concise, elegant, and
understandable code. It fits well in a “wholemeal” programming style, where we
think about composing together successive high-level transformations of a data
structure.

Consider the following string processing example, where we count the number
of characters that are not upper or lower case `c`:

```Haskell
numNotC :: String -> String
numNotC s = length (filter (== ‘C’) (toUpper s))
```

We can rewrite this as:

```Haskell
numNotC :: String -> String
numNotC = length . filter (== ‘C') . toUpper
```

This version makes it clearer what is really going on: `numNotC` is just a
“pipeline” composed of three smaller functions.

Notice that the second version omits the argument `s`. We discuss why this is
below.

## Partial application

Consider this example:

```Haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

The type of `f` might seem strange. Why all the arrows, instead of something
like `Int Int -> Int`? The answer is that all functions in Haskell actually
only take one argument! (For convenience, we will continue to refer to
functions that return functions as multi-argument functions.)

In the above example, `f` takes one argument, of type `Int`, and outputs a
function of type `Int -> Int`. That function then takes one argument and
finally returns an Int. In fact, we can equivalently write `f`’s type like
this:

```Haskell
f :: Int -> (Int -> Int)
```

That is, `->` is _right-associative_, so `W -> X -> Y -> Z` is equivalent to
`W -> (X -> (Y -> Z))` for any `W`, `X`, `Y`, `Z`. We can always add or remove
parentheses around the rightmost top-level arrow in a type. Conversely, if we
add parentheses around the leftmost top-level arrow, it changes the meaning.
`X -> Y -> Z` means the function takes in an `X` and a `Y` and returns a `Z`,
or equivalently, takes in an `X` and returns a function from `Y` to `Z`.
`(X -> Y) -> Z` means the function takes in a _function_ from `X` to `Y`
and returns a `Z`. For example, `foldr` has type
`(a -> b -> b) -> b -> [a] -> b`, and the first argument has to be a function.
If it had type `a -> b -> b -> b -> [a] -> b`, that would be a 5-argument
function taking in an `a`, three `b`s, and a list of `a`s.

Function application, on the other hand, is left-associative. That is,
`add 3 4` is really shorthand for `(add 3) 4`.  This is consistent with what we
said  previously: we apply `add` to an argument `3` and get a function that
takes in an `Int`, in this case `4`, and adds it to `3`.

A note on syntax: when we write `foo x y = ...`, this is really syntactic
sugar for `foo = \x y -> ... `. This in turn is just syntactic sugar for a
series of anonymous functions, in this case, `foo = \x -> (\y -> ...)`.
(By syntactic sugar, we just mean Haskell allows us to write the more
readable version, but it converts it to the second version when compiling.)
`\x -> (\y -> (\z -> ...))`.

With this knowledge of associativity, we can rewrite the type of our `compose`
function:

```Haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```

### Currying

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

These two styles are conceptually equivalent, and the reverse conversion is
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
argument to `filter`. This would be much harder to do if `filter` took both its
arguments in together as a pair!

In general, it is hard in Haskell to partially apply a function to any argument
other than the first. The one exception is infix operators. As we’ve seen,
these can be partially applied to either of their two arguments using an
_operator section_, such as `(> 100)` in the above example. This is because
with an infix operator, the compiler can tell which argument is being supplied
by which side of the operator it is on. `(> 100)` is clearly supposed to be
`\x -> x > 100`, and `(10 -)` is clearly supposed to be `\x -> 10 - x`, because
of which side of the operator we put `100` and `10` on. With prefix operators,
we do not have two sides to distinguish arguments with. For example, with
`div 8`, the compiler would have no way of determining if `8` is supposed to be
the first or second argument, so to avoid ambiguity it always interprets the
first argument supplied as the first argument to the function.

When writing a multi-argument function intended to be used as a prefix,
however, it can make a huge difference which order you choose to put the
arguments in! Think about which argument you are most likely to want to
partially apply the function to. For example, with `filter`, we often want to
filter several lists using the same criterion, and we rarely want to filter the
same list using multiple criteria, so it makes sense for its function argument
to come first.

### Eta reduction
Eta reduction is the process of removing unnecessary abstractions when the
argument then immediately gets passed as the argument to another function.
That is, it is the conversion of functions like `\x -> f x` to just `f`.

Recall that we said all functions are lambda abstractions, i.e.,
`add x y = x + y` is really `add = \x -> (\y -> x + y)`. We can use this
fact to simplify many function definitions using eta reduction. Consider
`sum xs = foldr (+) 0 xs`. This is really `sum = \xs -> foldr (+) 0 xs`,
so we can eta reduce and write this as `sum = foldr (+) 0`. This makes
sense, because both `sum` and `foldr (+) 0` are functions that take in
a list of `Int`s and return an `Int`, so we can define the first directly in
terms of the second.

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
