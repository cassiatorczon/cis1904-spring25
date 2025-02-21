# Polymorphism

## Polymoprphic data types

Recall this data type from the algebraic data type reading:

```Haskell
data FailableDouble
  = Failure
  | OK Double
```

And this data type from the exercises:

```Haskell
data WeatherForecast
  = Forecast Weather
  | Unknown
```

These look suspiciously similar! It turns out Haskell has an abstraction for
this pattern:

```Haskell
data Maybe a
  = Nothing
  | Just a
```

A `Maybe a` is, possibly, an `a`. It's an abstraction that helps us represent
_partial computation_, i.e., computations that may have no result on some
inputs.

Instead of `FailableDouble`, we could use `Maybe Double`, and instead of
`WeatherResult`, we could use `Maybe Weather`, to indicate that when a
computation succeeds, it will produce a `Double` (or `Weather`, respectively),
but it may also fail on a given input, resulting in `Nothing`.

We discussed in the algebraic data type material that `data` is a keyword for
defining _type constructors_. Most of the type constructors we have seen so far
have had no arguments, so we just called them types.
`Maybe` is a type constructor with an argument. To make it a type,
we must supply `Maybe` with another type, like `Double` or `Weather`. When we
do so, we simply replace all uses of `a` in `Maybe`’s definition with the type
chosen as the parameter. We've seen many examples of substituting terms in for
other terms, for example with data constructors. Here we just apply this same
principle to types.

Here is some sample code using `Maybe`:

```Haskell
toDouble :: Maybe Double -> Double -> Double
toDouble (Just n) _ = n
toDouble Nothing default = default

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv m n = Just (m / n)
```

Let’s look at another example:

```Haskell
data List a
  = Nil
  | Cons a (List a)
```

Given a type `a`, a `List a` consists of either the constructor `Nil`, or the
constructor `Cons` along with a value of type `a` and another `List a`.
Here is an example:

```Haskell
sumInts :: List Int -> Int
sumInts Nil = 0
sumInts (Cons h tl) = h + sumInts tl
```

This `List` type is exactly like the built-in list type, only without special
syntax. In fact, when you say `[Int]` in a type, that really means `[] Int`
— allowing you to put the brackets around the `Int` is just a nice syntactic
sugar.

## Polymorphic functions

Let’s say we want to retrieve the first element of a list, but we need to be
able to return something if we have an empty list. Our return type will thus
involve a `Maybe`. The type of the list elements does not matter here, so our
function can be polymorphic, and the full return type will thus be `Maybe a`.

```Haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
```

## Total and partial functions

Unfortunately, the version of this function in Prelude, called `head`, instead
has type `[a] -> a`. It crashes when given an empty list!

`head` is a _partial function_: there are certain inputs for which `head` will
crash. Functions that recurse infinitely on some inputs are also considered
partial. Functions which are well-defined (i.e., produce an output)
on all possible inputs are called _total functions_.

Avoiding partial functions is good practice in any programming language. In
Haskell, structures like `Maybe` make this easier.

In addition to `head`, other Prelude partial functions include `tail`, `init`,
`last`, and `(!!)`. You should avoid using partial functions. What to do
instead?

Often uses of partial functions like `head`, `tail`, and so on can be replaced
by pattern-matching. Consider the following two definitions:

```Haskell
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1 : x2 : _) = x1 + x2
```

These functions compute exactly the same result, and they are both total. The
second, however, is easier to read, and because it contains no partial
functions, we can be sure that it will not crash even if we accidentally
mix up our cases.

We can also use alternatives like `safeHead` if we truly need the functionality.
Why is this a good idea?

1. `safeHead` will never crash.
2. The type of `safeHead` makes it obvious that some inputs conceptually have no
    output for this function, hence the need for `Maybe`. The type of `head`,
    conversely, gives no indication that it may fail.
3. The type system ensures that users of `safeHead` must appropriately check
    the return value of `safeHead` to see whether they got a value or
    `Nothing`.
