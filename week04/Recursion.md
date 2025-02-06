# Recursion Patterns

So far, we have seen several recursive functions, especially with lists.
However, experienced Haskell programmers often avoid directly writing
recursive functions in their code.

How is this possible? The key is to notice that there are certain common
patterns that come up over and over again. By abstracting these patterns out
into library functions, programmers can leave the low-level details of actually
doing recursion to these functions and instead focus on high-level, wholemeal
programming. This can help keep code readable, improve modularity, and avoid
minor implementation errors.

## Map

Suppose we wanted to take the absolute value of every number in a list:

```Haskell
absAll :: [Int] -> [Int]
absAll [] = []
absAll (x : xs) = abs x : absAll xs
```

Next, suppose we wanted to square every number:

```Haskell
squareAll :: [Int] -> [Int]
squareAll [] = []
squareAll (x : xs) = x * x : squareAll xs
```

Third, suppose we want to add three to every number:

```Haskell
add3All :: [Int] -> [Int]
add3All [] = []
add3All (x : xs) = x + 3 : add3All xs
```

These functions look suspiciously similar! In fact, the only thing that differs
is the operation applied to the individual elements. Consider our first attempt
at a `map` function:

```Haskell
-- Attempt 1:
map :: (Int -> Int) -> [Int] -> [Int]
map _ [] = []
map f (x : xs) = f x : map f xs
```

Observe that `map` takes a function as a parameter. In Haskell, functions are
_first class_, so they can be passed around just like any other type of data.

Using `map`, we can now write

```Haskell
absAll :: [Int] -> [Int]
absAll xs = map abs xs

squareAll :: [Int] -> [Int]
squareAll xs = map (\x -> x * x) xs

add3All :: [Int] -> [Int]
add3All xs = map (+ 3) xs
```

The function we pass into `map` can either be named, such as `abs`, or
_anonymous_, such as `(\x -> x * x)`, the counterpart to `fun x -> x * x` in
OCaml. For historical reasons (look up the lambda calculus if you're curious!),
we read the backslash as "lambda".

The third example uses an _operator section_. If `?` is an operator, then
`(? y)` is equivalent to the function `\x -> x ? y`, and `(y ?)` is equivalent
to `\x -> y ? x`.

There's no reason to restrict ourselves to integer lists. Mapping over lists of
other types, like booleans or strings, would look the same. In fact, the only
difference to the definition of `map` would be the type signature.

Thankfully, Haskell supports _polymorphism_. The word "polymorphic" comes from
Greek and means "having many forms": something which is polymorphic works for
multiple types. Consider this more general type signature:

```Haskell
-- Attempt 2:
map :: (a -> a) -> [a] -> [a]
```

Here, `a` is a _type variable_, which can stand for any type.

We have made good progress, but we still have the restriction that we always
get a list with the same type of elements as the starting list. What if we want
to map with a function that returns a different type than its input?
To do this, we need a second type variable:

```Haskell
map :: (a -> b) -> [a] -> [b]
```

This says, for any types `a` and `b`, `map` takes in a function from `a` to `b`
and a list of `a`s as input and returns a list of `b`s as output.

With this final version, we can use `map` to, for example, round decimal
numbers to integers:

```Haskell
roundAll :: [Double] -> [Int]
roundAll xs = map round xs
```

## Filter

Frequently we want to filter a list, i.e., keep some of its elements and throw
the rest away, based on a test. For example, we might want to keep only the
uppercase characters in a `String` (i.e., a `[Char]`):

```Haskell
upperOnly :: [Char] -> [Char]
upperOnly [] = []
upperOnly (x : xs)
  | isUpper x = x : upperOnly xs
  | otherwise = upperOnly xs
```

Or only the positive numbers in a list of integers:

```Haskell
positiveOnly :: [Int] -> [Int]
positiveOnly [] = []
positiveOnly (x : xs)
  | x > 0 = x : positiveOnly xs
  | otherwise = positiveOnly xs
```

Again, we can abstract out this pattern into a more general-purpose function:

```Haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
  | f x = x : filter xs
  | otherwise = filter xs
```

Then, the two previous examples become

```Haskell
upperOnly :: [Char] -> [Char]
upperOnly xs = filter isUpper xs

positiveOnly :: [Int] -> [Int]
positiveOnly xs = filter (> 0) xs
```

Notice how this is both more concise and self-documenting; the name `filter`
makes it easy to understand what this code is doing.

## Fold

Finally, sometimes we want to combine all the elements of a list in some way.
Consider these examples:

```Haskell
sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x : xs) = x * product xs

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs
```

For these, we can use the `fold` function:

```Haskell
fold :: (a -> b -> b) -> b -> [a] -> b
fold f z [] = z
fold f z (x : xs) = f x (fold f z xs)
```

We can now rewrite our examples.

```Haskell
sum :: [Int] -> Int
sum xs = fold (+) 0 xs

product :: [Int] -> Int
product xs = fold (*) 1 xs

length :: [a] -> Int
length xs = fold (\_ l -> 1 + l) 0 xs
```

`fold` traverses the list element by element, maintaining an _accumulator_ that
records the result so far. The first argument to `fold` describes how the next
element of the list at any given point should be combined with the accumulator.
The second argument gives the starting value for the accumulator.

There are two basic versions of `fold`: `foldr` and `foldl`.
(The definition shown above is actually `foldr`.)
The `r` and `l` stand for right- and left-associative.

```Haskell
foldr f z [a,b,c] == f a (f b (f c z))

foldl f z [a,b,c] == f (f (f z a) b) c
```

For non-associative functions or non-commutative functions, such as subtraction
(non-associative, non-commutative), binary averaging
(commutative, non-associative), or `append` (associative, non-commutative),
these are conceptually very different.
For example:

```Haskell
foldr (-) 0 [1,1,1] == 1
foldl (-) 0 [1,1,1] == -3

foldr (\x y -> (x + y) / 2) 0 [8,4,16] == 7
foldl (\x y -> (x + y) / 2) 0 [8,4,16] == 10

foldr (++) "z" ["a","b","c"] == "abcz"
foldl (++) "z" ["a","b","c"] == "zabc"
```

For functions that are associative and commutative, such as `+`, we have a
choice of which to use. In Haskell, we primarily use `foldr` in this scenario.

If you are used to imperative code, this might feel backward!
The equivalent definition `foldr` of in imperative pseudocode would be
something like:

```
foldr(f,z,lst) =
  acc = z;
  for (i = len(lst) - 1; i >= 0; i--):
    acc = f(lst[i],z)
  return acc
```

compared to `foldl`:

```
foldr(f,z,lst) =
  acc = z;
  for (i = 0; i < len(lst); i++):
    acc = f(z,lst[i])
  return acc
```

Notice the order of the iteration. Why do we do prefer `foldr` in Haskell?

Consider our earlier `fold` example.

```Haskell
foldr f z [a,b,c] == f a (f b (f c z))

foldl f z [a,b,c] == f (f (f z a) b) c
```

Observe that `foldr` naturally mirrors the structure of the list:
`Cons a (Cons b (Cons c []))` becomes `f a (f b (f c z))`.
It follows the natural way we destruct lists in Haskell, combining
the head with the result of recursively operating on the tail.

This is a great example of how the fundamental data structues of
functional code lead to different idioms than those used in other
programming paradigms!

`foldr` also allows for short-circuiting in a way that is not possible
with `foldl`. In some cases, we can even use `foldr` with infinite lists!
We will talk more about this in the laziness lecture.

One final note: `foldr` and `foldl` can both run into space complexity issues.
(While `foldl` looks tail recursive, Haskell's lazy evaluation strategy means
that it is actually often less space efficient than `foldr`.)
To manage this, Haskell provides a third option: `foldl'`.
This conceptually behaves the same as `foldl` but also carefully manages which
parts of the computation evaluate when. In nearly all situations where you might
use `foldl`, `foldl'` is the better choice.

If you'd like to learn more about the distinction, a helpful discussion
can be found here: https://wiki.haskell.org/Foldr_Foldl_Foldl'.
(If you click this link directly, it may not include the final ' depending on
your editor. If the link seems to be broken, make sure the ' is there.)