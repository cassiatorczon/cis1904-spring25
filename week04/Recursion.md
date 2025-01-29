# Recursion Patterns

So far, we have written a lot of explicitly recursive functions, especially when using lists. But in fact, experienced Haskell programmers hardly ever write recursive functions!

How is this possible? The key is to notice that there are certain common patterns that come up over and over again. By abstracting these patterns out into library functions, programmers can leave the low-level details of actually doing recursion to these functions and instead focus on high-level, wholemeal programming.

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

These functions look suspiciously similar! In fact, the only thing that differs is the operation to compute the individual elements. Consider our first attempt at a `map` function:

```Haskell
-- Attempt 1: 
map :: (Int -> Int) -> [Int] -> [Int]
map _ [] = []
map f (x : xs) = f x : map f xs
```

Observe that `map` takes a function as a parameter. In Haskell, functions are _first class_, so they can be passed around just like any other type of data.

Using `map`, we can now write

```Haskell
absAll :: [Int] -> [Int]
absAll xs = map abs xs

squareAll :: [Int] -> [Int]
squareAll xs = map (\x -> x * x) xs

add3All :: [Int] -> [Int]
add3All xs = map (+ 3) xs
```

The function we pass into `map` can either be named, such as `abs`, or _anonymous_, such as `(\x -> x * x)`. 
The third example uses an _operator section_. If `?` is an operator, then `(? y)` is equivalent to the function `\x -> x ? y`, and `(y ?)` is equivalent to `\x -> y ? x`.

There's no reason to restrict ourselves to integer lists. Mapping over lists of other types, like booleans or strings, would result in exactly the same code. In fact, the only difference would be the type signature. 

Thankfully, Haskell supports _polymorphism_. The word "polymorphic" comes from Greek and means "having many forms": something which is polymorphic works for multiple types. Consider this more general type signature:

```Haskell
-- Attempt 2: 
map :: (a -> a) -> [a] -> [a]
```

Here, `a` is a _type variable_, which can stand for any type.

We have made good progress, but we still have the restriction that we always get a list with the same type of elements as the starting list. To loosen this restriction, we need a second type variable:

```Haskell
map :: (a -> b) -> [a] -> [b]
```

This says, for any types `a` and `b`, `map` takes in a function from `a` to `b` and a list of `a`s as input and returns a list of `b`s as output. 

With this final version, we can use `map` to, for example, round decimal numbers to integers:

```Haskell
roundAll :: [Double] -> [Int]
roundAll xs = map round xs
```

## Filter

Another common pattern is when we want to keep some elements of a list and throw others away, based on a test. For example, we might want to keep only the uppercase characters:

```Haskell
upperOnly :: [Char] -> [Char]
upperOnly [] = []
upperOnly (x : xs)
  | isUpper x = x : upperOnly xs 
  | otherwise = upperOnly xs
```

Or only the positive numbers:

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

## Fold

The third pattern we will talk about is combining the elements of a list into a final answer. Consider these examples:

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

We can define an extremely powerful `fold` function: 

```Haskell
fold :: (a -> b -> b) -> b -> [a] -> b
fold f z [] = z
fold f z (x : xs) = f x (fold f z xs)
```

This allows us to write

```Haskell
sum :: [Int] -> Int
sum xs = fold (+) 0 xs

product :: [Int] -> Int
product xs = fold (*) 1 xs

length :: [a] -> Int
length xs = fold (\_ l -> 1 + l) 0 xs
```

We will discuss the intuition behind `fold` in a lot more detail during class. For now, observe that `fold` essentially replaces `[]` with `z` and `(:)` with `f`. That is,

```Haskell
fold f z (a : b : c : []) == a `f` (b `f` (c `f` z))
```

The `fold` we have defined here is actually named `foldr` in Haskell, to distinguish it from `foldl`. We will discuss the difference in class!
