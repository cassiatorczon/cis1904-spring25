# Functor and Foldable

This week, we will see how to map and fold over more than just lists, with the
help of the type class machinery that we introduced last week.

## A brief digression on kinds

Just as every expression has a type, types themselves have “types,” called
kinds. (Before you ask: no, there’s not another level beyond kinds — in
Haskell.) In `GHCi` we can ask about the kinds of types using `:kind`. For
example, let’s ask for the kind of `Int`:

```Haskell
Prelude> :k Int
Int :: *
```

We see that `Int` has kind `*`. In fact, every type which can actually serve as
the type of some values has kind `*`.

```Haskell
Prelude> :k Bool
Bool :: *
Prelude> :k Char
Char :: *
Prelude> :k Maybe Int
Maybe Int :: *
```

If `Maybe Int` has kind `*`, then what about `Maybe`? Notice that there are no values of type `Maybe`. There are values of type `Maybe Int`, and of type `Maybe Bool`, but not of type `Maybe`. However, `Maybe` is certainly a valid type-like-thing. So what is it? What kind does it have? Let’s ask `GHCi`.

```Haskell
Prelude> :k Maybe
Maybe :: * -> *
```

`Maybe` is, in a sense, a function on types — we usually call it a type constructor. It takes as input types of kind `*`, and produces another type of kind `*`. For example, it can take as input `Int :: *` and produce the new type `Maybe Int :: *`.

Are there other type constructors with kind `* -> *`? Sure. For example, the list type constructor, written `[]`, or `Tree`.

## Functor

Suppose we want to generalize `map`, so that it works not only for lists:
```Haskell
(a -> b) -> [a] -> [b]
```
but also `Maybe`s:
```Haskell
(a -> b) -> Maybe a -> Maybe b
```
and `Tree`s:
```Haskell
(a -> b) -> Tree a -> Tree b
```

The essence of this mapping pattern is a higher-order function with a type like

```Haskell
(a -> b) -> f a -> f b
```
where `f` is a type variable standing in for some type of kind `* -> *`. So, can we write a function of this type once and for all?

Well, not really. There’s not much we can do if we don’t know what `f` is. The solution is to make a type class, which is traditionally called `Functor`:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Now we can just implement this class in a way specific to each particular `f`. Note that the `Functor` class abstracts over types of kind `* -> *`. So it would make no sense to write

```Haskell
instance Functor Int where
  fmap = ...
```

However, it does make sense (kind-wise) to make a `Functor` instance for, say, `Maybe`. Let’s do it. Following the types makes it almost trivial:

```Haskell
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
```

How about lists?

```Haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap f (x : xs) = f x : fmap f xs
  -- or just
  -- fmap = map
```

## Foldable

Analogously, we might want to generalize `foldr` beyond lists. We can accomplish this with the `Foldable` type class. `Foldable` contains many functions, but we'll focus on just one:

```Haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
```

Notice that this `foldr` has the same type signature that we're used to, but it's generalized over any container `t`. Similarly to before, we want a `t` with kind `* -> *`.

We can write a `Foldable` instance for lists:

```Haskell
instance Foldable [] where
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr = -- same implementation as we saw previously
```

Or for trees:

```Haskell
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Branch l x r) = foldr f (f x (foldr f z r)) l
```

Here, we first fold the right subtree `r`, and then combine the result with the node value `x`, and then pass that result as the starting accumulator to fold the left subtree `l`.

Using a type class like `Foldable`, we can write general functions like this one, which can transform a value of *any* type that is an instance of `Foldable` into a list:

```Haskell
toList :: Foldable t => t a -> [a]
toList = foldr (:) []
```

Notice the use of the type class constraint `Foldable t =>` to enforce that `t` implements `Foldable`.
