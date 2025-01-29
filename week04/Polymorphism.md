# Polymorphism

## Polymoprphic data types

Recall this data type from the reading for Week 2:

```Haskell
data FailableDouble
  = Failure
  | OK Double
```

And this data type from Class 2:

```Haskell
data WeatherResult
  = Invalid
  | Valid Weather
```

These look suspiciously similar! It turns out that we can write this more directly:

```Haskell
data Maybe a
  = Nothing
  | Just a
```

A `Maybe a` is, possibly, an `a`. So, instead of `FailableDouble`, we could use `Maybe Double`, and instead of `WeatherResult`, we could use `Maybe Weather`.

`Maybe` is a _type constructor_. To become a proper, full-blooded type, we must supply `Maybe` with another type, like `Double` or `Weather`. When we do so, we simply replace all uses of `a` in `Maybe`’s definition with the type chosen as the parameter. We’re quite used to thinking about substituting terms in for other terms (that’s what we use variables for!), and here we just apply this same principle to types.

Here is some sample code:

```Haskell
toDouble :: Maybe Double -> Double
toDouble (Just n) = n
toDouble Nothing = 0

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

Given a type `a`, a `(List a)` consists of either the constructor `Nil`, or the constructor `Cons` along with a value of type `a` and another `(List a)`. Here are some examples:

This `List` type is exactly like the built-in list type, only without special syntax. In fact, when you say `[Int]` in a type, that really means `[] Int` — allowing you to put the brackets around the `Int` is just a nice syntactic sugar.

## Polymoprhic functions

We've already seen polymorphic functions in the context of `map`, `filter`, `fold`, and so on. Here's another example:

Let’s say we want retrive the first element of a list. But, we don’t know whether that list has any elements at all, and we need to be able to return something if we have an empty list. Our return type will thus be a `Maybe`.

```Haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
```

## Total and partial functions

Unfortunately, the version of this function in Prelude, called `head`, instead has type `[a] -> a`. It crashes when given an empty list!

`head` is a _partial function_: there are certain inputs for which `head` will crash. Functions which have certain inputs that will make them recurse infinitely are also partial. Functions which are well-defined on all possible inputs are _total functions_.

It is good Haskell practice to avoid partial functions as much as possible. Actually, avoiding partial functions is good practice in any programming language — but Haskell tends to make this easier.

In addition to `head`, other Prelude partial functions include `tail`, `init`, `last`, and `(!!)`. You should avoid using partial functions. What to do instead?

Often uses of partial functions like `head`, `tail`, and so on can be replaced by pattern-matching. Consider the following two definitions:

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

These functions compute exactly the same result, and they are both total. But only the second one is _obviously_ total, and it is much easier to read anyway.

Also, instead of `head`, we can use alternatives like `safeHead` if we truly need the functionality. Why is this a good idea?

1. `safeHead` will never crash.
2. The type of `safeHead` makes it obvious that it may fail for some inputs.
3. The type system ensures that users of `safeHead` must appropriately check the return value of `safeHead` to see whether they got a value or `Nothing`.
