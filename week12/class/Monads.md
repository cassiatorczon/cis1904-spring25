# Monads

## Motivation

Monads are a construct that functional programmers have borrowed from
_category theory_ to model side effects.

A monad is handy whenever a programmer wants to sequence _actions_, or
effectful code. In Haskell, monads are represented as a typeclass with two
functions (and a third, but typically we omit this because the default
implementation suffices). How those functions are defined determines how that
effect gets sequenced.

We’ve already (implicitly) learned about three monads: `Maybe`, `List`, and
`IO`, which model program crashes, nondeterminism, and input/output. (Of
course, lists have many uses beyond modelling nondeterminism; that is just how
we think of them in the conceptual framework of effects). We will look more
closely at their shared structure below.


## Monad

The `Monad` type class is defined as follows:

```Haskell
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2
```

We have already seen these functions in the context of `IO`, but they are
actually part of the typeclass instantiation for every monad.

`(>>)` is just a specialized version of `(>>=)`. It is included in the `Monad`
class in case we want to provide a more efficient implementation for specific
instances, but usually we just use a default implementation written in terrms
of `(>>=)`. For this reason, we will focus on `(>>=)`.

`(>>=)` (pronounced “bind”) is what lets us sequence actions. Look carefully
at its type `m a -> (a -> m b) -> m b`.

`(>>=)` takes two arguments. The first one is a value of type `m a`. (These are
sometimes called monadic values, or computations. A common mistake is to call
something of type `m a` a monad, but that is a kind error: the type constructor
`m` is the monad.) An action of type `m a` represents a computation which
results in a value (or several values, or no values) of type `a`, and typically
also has some sort of side effect:

-   `c1 :: Maybe a` is a computation which might fail but produces an `a` if it
    succeeds.
-   `c2 :: [a]` is a computation which can produce an arbitrary number of `a`s.
    The effect interpretation of this is that it is modelling all the potential
    outputs of a nondeterministic program.
-   `c3 :: IO a` is a computation which potentially has some I/O effects and
    then produces an `a`.
-   `c4 :: Rand StdGen a` is a computation which may use pseudo-randomness and
    produces an `a`.

And so on. Now, what about the second argument to `(>>=)`? It is a function of
type `(a -> m b)`. That is, it is a function which will choose the next
computation to run based on the result(s) of the first computation. This is
precisely how we can sequence computations with `Monad`.

So all `(>>=)` really does is put together two actions to produce a larger one,
which first runs one and then the other, returning the result of the second.
Crucially, we can decide which action to run second based on the output from
the first, i.e., the result of the first can affect later control flow.

The default implementation of `(>>)` is just a trivial case of `(>>)`:
`m1 >> m2` simply does `m1` and then `m2`, ignoring the result of `m1`.

What about `return`? We saw with `IO` that this converts an expression into
an instruction to create that expression. `return` is similar with other
monads: it takes an expression and makes an "effectful" version of it.
That is, it tells us how to view that expression within our model of a given
effect. (Note: a common pitfall is thinking of `return` like a return
statement in other languages; it is actually just an expression that can
go anywhere within a computation and has nothing to do with exiting a
function.)

## Examples

Let’s start by writing a `Monad` instance for `Maybe`:

```Haskell
instance Monad Maybe where
  return  = Just

  Nothing >>= _ = Nothing
  Just x  >>= f = f x
```

`return` is `Just`. If we have some concrete value, say `3`, then we can view
that within our effect model as a "successful" computation producing 3, as
opposed to a computation that failed or produced some other value.
(Remember that we are _modelling_ effects here. `Nothing` is a representation
of failure; the program will not literally crash if it returns `Nothing`.)

`(>>=)` sequences two potentially-failing computations. If the first argument
of `(>>=)` is Nothing, then the whole computation fails; otherwise, if it is
`Just x`, we continue the program by applying the second argument to `x`.

Some examples:

```Haskell
checkPos :: Int -> Maybe Int
checkPos n
  | n > 0 = Just n
  | otherwise = Nothing

halve :: Int -> Maybe Int
halve n
  | even n = Just (n `div` 2)
  | otherwise = Nothing

ex1 :: Maybe Int
ex1 = checkPos 7 >>= halve

ex2 :: Maybe Int
ex2 = check -2 >>= halve
```

The `do` notation we’ve learned for working with `IO` can work with any monad.
As in `IO`, `<-` followed by a line break is like a combination of `\` and
`(>>=)`. In the example below, `check 7` is the first effectful computation,
and `<-` binds its result to the name `checked` before continuing in the
computation in the next line. (Because `(>>)` ignores the result of its first
argument, lines without `<-` are understood as having `(>>)` rather than
`(>>=)` between them and the next line.)

```Haskell
ex1' = do
  checked <- check 7
  halve checked

ex2' = do
  checked <- check 12
  halve checked
```

(Think about what these examples should evaluate to!)

How about a `Monad` instance for the list constructor `[]`?

```Haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
```

A value `x` is represented in our nondeterminism model as a singleton list
containing `x`. Conceptually, we are modelling situations where there is
exactly one possible value of our program at a given point, and it is `x`.

Consider a point mid-program where we could have several possible values,
represented by a list `xs`. What possible values could we have after the next
step of our program (represented by some function `f`)? To find out, we
run `f` on each of our current possible values (using `map`). However, `f`
could itself be nondeterministic, so for eacch `x` in `xs`, we get another
list from applying `f`. To see all the values we could have after that
step, we concatenate the lists we get from `map f xs`.

A simple example:

```Haskell
addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex3 = do
  num <- [10, 20, 30]
  addOneOrTwo num
```

`num` is non-deterministically selected from `[10, 20, 30]` and then is
non-deterministically added to `1` or `2`. The result is a list of 6 elements
representing all possible results.

This non-determinism can be made even more apparent through the use of the
function `guard`, which ends a given "trace" of nondeterministic computation
if its argument isn’t `True`.
`guard` is polymorphic, but in this case, we can think of it as a function that
returns `[]` if its argument is false, and otherwise returns `[()]`. The
difference is that the first represents an end to a given trace, while the
second has no interesting result, but keeps a "spot" in the list, i.e.,
keeps that trace going.

```Haskell
ex4 = do
  num <- [1..20]
  guard (even num)
  guard (num `mod` 3 == 0)
  return num
```

A note about associativity: the above desugars to
`[1..20] >>= (\num -> (guard (even num) >> (guard (mod num 3 == 0) >> return num)))`.
This means that even though `guard` has no result, we can still use results
from previous steps in steps that occur after we use `guard`.

We can think of `ex4` as filtering `[1..20]` for even numbers divisible by 3.

(The full type of guard is `MonadPlus m => Bool -> m ()`. `MonadPlus` is
another type class that characterizes monads that have a possibility of
failure. These include `Maybe` and `[]`.)

In the `IO` unit, we said that everything within a `do` block must be an
instruction, i.e., within the IO monad. (Note that not everything needs to
be the same _type_: the type argument to `IO` can be different.) In general,
it is important for everything in a `do` block to be within the _same_ monad;
we cannot mix some lines in the `Maybe` monad with some lines in the `List`
monad (though, as usual, we can have a list containing `Maybe`s or a `Maybe`
containing a list).

## Monad combinators

One nice thing about the Monad class is that, using only `return` and `(>>=)`,
we can build up a lot of nice general combinators for programming with monads.

For example, `sequence` takes a list of monadic values (remember, these are
values of type `m a` where `m` is a monad) and produces a single
monadic value which collects the results. What this means depends on the
particular monad. For example, in the case of `Maybe` it means that the entire
computation succeeds only if all the individual ones do; in the case of `IO` it
means to run all the computations in sequence; and so on.

```Haskell
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (mx : mxs) =
  mx >>= \x ->
  sequence mxs >>= \xs ->
  return (x : xs)
```

We'll see several more examples of monad combinators in class and on the
homework.

## List comprehensions

The monad for lists gives us a new notation for list building that turns out to
be quite convenient. Building lists using monad-like operations is so useful
that Haskell has a special syntax for it, called list comprehensions:

```Haskell
upTo50 :: [Int]
upTo50 = [n `div` 2 | n <- [1..100], even n]

-- inefficient, but it works
primes :: [Int]
primes = [p | p <- [2..],
              all ((/= 0) . (p `mod`)) [2..p-1] ]
```

`evensUpTo100` filters `[1..100]` for even numbers and divides each resulting
element by 2. It is equivalent to:

```Haskell
upTo50' :: [Int]
upTo50' = do
  n <- [1..100]
  guard (even n)
  return (n `div` 2)
```

`primes` is the infinite list of all prime numbers. It is equivalent to:

```Haskell
primes' :: [Int]
primes' = do
  p <- [2..]
  guard (all ((/= 0) . (p `mod`)) [2..p-1])
  return p

```

List comprehensions are very similar to set notation you may have learned in
a math class. In a list comprehension, the statements to the right
of the `|` are carried out, in order. A statement with a `<-` selects an
element from a list. Statements without `<-` are boolean expressions; if the
expression is `False`, then the current choice of elements is thrown out.

There is a straightforward translation between list comprehensions and do
notation:

```Haskell
[x | y <- ys, g1, g2, z <- zs, g3]
```

is exactly equivalent to

```Haskell
do
  y <- ys
  guard g1
  guard g2
  z <- zs
  guard g3
  return x
```

Note that, in the translation, lists aren’t mentioned anywhere! With the GHC
language extension `MonadComprehensions`, you can use list comprehension
notation for any monad.

## Monad transformers
These are beyond the scope of this class, but we can combine multiple monads
into one using _monad transformers_. These are very useful for writing
real-world Haskell.