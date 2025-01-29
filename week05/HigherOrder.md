# Higher-Order Patterns

## Function composition

Just as we can write functions that take other functions as inputs, we can also write functions that output functions. Consider this:

```Haskell
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)
```

As it turns out, `compose` represents function composition and in the standard library, we instead call it `(.)`. That is, if `f` and `g` are functions, then `f . g` is the function which first does `g` and then `f`.

Function composition can be quite useful in writing concise, elegant code. It fits well in a “wholemeal” style where we think about composing together successive high-level transformations of a data structure.

As an example, consider the following function:

```Haskell
myTest :: [Int] -> Bool
myTest xs = even (length (greaterThan100 xs))
```

We can rewrite this as:

```Haskell
myTest :: [Int] -> Bool
myTest = even . length . greaterThan100
```

This version makes it clearer what is really going on: `myTest` is just a “pipeline” composed of three smaller functions.

## Partial application

Consider a function like this:

```Haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

The type of `f` might have struck you as a little strange. Why all the arrows, instead of something like `Int Int -> Int`? There's actually a beautiful, deep reason for this.

All functions in Haskell take only one argument! What? But doesn’t the function `f` shown above take two arguments? No, actually, it doesn’t: it takes one argument (of type `Int`) and outputs a function (of type `Int -> Int`); that function takes one argument and returns the final answer. In fact, we can equivalently write `f`’s type like this:

```Haskell
f :: Int -> (Int -> Int)
```

In particular, note that function arrows associate to the right, that is, `W -> X -> Y -> Z` is equivalent to `W -> (X -> (Y -> Z))`. We can always add or remove parentheses around the rightmost top-level arrow in a type.

Function application, in turn, is left-associative. That is, `f 3 4` is really shorthand for `(f 3) 4`. This makes sense given what we said previously: we apply `f` to an argument `3`, which returns a function that takes an `Int` and adds 6 to it.

The “multi-argument” anonymous function `\x y z -> ...` is really just syntactic sugar for `\x -> (\y -> (\z -> ...))`.

The type of our composition function can equivalently be written as `(b -> c) -> (a -> b) -> a -> c`, and we can have

```Haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```

This idea of representing multi-argument functions as one-argument functions returning functions is known as _currying_, named for the British mathematician and logician Haskell Curry. (They were in fact first discovered by Moses Schönfinkel.)

The fact that functions in Haskell are curried makes _partial application_ particularly easy, where we supply some arguments and get a function out of the remaining arguments. For example, we can partially apply `filter` to write

```Haskell
greaterThan100 :: [Int] -> [Int]
greaterThan100 = filter (> 100)
```

Haskell doesn’t make it easy to partially apply to an argument other than the first. The one exception is infix operators, which as we’ve seen, can be partially applied to either of their two arguments using an operator section. There is an art to deciding the order of arguments to a function to make partial applications of it as useful as possible.

## Another example

Let's tie some of the things we've learned together. Consider

```Haskell
foobar :: [Int] -> Int
foobar [] = 0
foobar (x : xs)
  | x > 3     = (7 * x) + foobar xs
  | otherwise = foobar xs
```

This seems straightforward enough, but it is not good Haskell style. The problem is that it is a) doing too much at once and b) working at too low a level. Instead of thinking about what we want to do with each element, we can think about making step-by-step transformations to the entire input, using the recursion patterns that we have. Here’s a much more idiomatic implementation:

```Haskell
foobar :: [Int] -> Int
foobar = sum . map (7 *) . filter (> 3)
```

This defines `foobar` as a “pipeline” of three functions: first, we throw away all elements from the list which are not greater than three; next, we apply an arithmetic operation to every element of the remaining list; finally, we sum the results.

Observe that `map` and `filter` have been partially applied, and we use `(.)` to compose the functions together.
