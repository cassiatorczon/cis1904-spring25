# Introduction to Type Classes

We saw in the polymorphism lecture that there are two main forms of
polymorphism in Haskell, parametric polymorphism and ad-hoc polymorphism. In
that class, we talked about parametric polymorphism. We will review that
quickly here before discussing ad-hoc polymorphism and typeclasses.

## Parametricity

Consider the type `a -> a -> a`. Remember that `a` is a type variable, so this
is really more like shorthand for `forall a, a -> a -> a`, i.e., any type may
be substituted in for `a` when we go to use this.

In _parametric polymorphism_, functions are _parameterized_ over one or more
types. Any function `f` of type `a -> a -> a` must do the same thing to its
input regardless of the value substituted in for `a`. (We say such a function
is _parametric_ in the type `a`.) In fact, Haskell actually _erases_ the types
before running a program: they are used in compilation, but the code cannot
check them at runtime!

What does it mean that a function must work for every value of `a`?
Let's look at the following example:

```Haskell
f :: a -> a -> a
f x y = x && y
```

The syntax looks reasonable, but this doesn't type check. We get an error
message that type `a` can't be matched with type `Bool`.

The reason this doesn’t work is that the _caller_ of a polymorphic function
gets to choose what `a` is (by passing in an argument). Here we, as the
_implementors_ of `f`, have tried to choose a specific type for `a` (namely,
`Bool`) by using `&&`, which expects two `Bool`s. This fails because the caller
could give us a `String`, or an `Int`, or even a custom type, which we couldn't
possibly know about in advance.

To put this another way, you can read the type `a -> a -> a` as a promise that
a function with this type will work regardless of what type the caller chooses
for `a`. (Note that the caller only gets to make this choice once per call --
the type of the second argument in this example must match the type of the
first.) Crucially, with parametric polymorphism, it is a promise that the
function will do _the same thing_ regardless of `a`.

For example, consider the type `a -> a`. How many functions can take a value,
of any arbitrary type, and in a uniform way return another value of the same
type?

Exactly one! If we don't know the type, there's nothing we can do besides
return our input, so this must be the identity function.

What about the type `a`? (Remember, this is implicitly `forall a, a`, i.e.,
this term must have every type.) How many terms have this type?

The answer is that, in Haskell, this is only used to represent error
behavior. This is why we can put `undefined` as the definition of any function:
it has every type! Outside of error behavior, however, it defeats much of the
purpose of types to have a term that is every type, so you will not see many
terms whose type is `forall a, a`.

As a more typical example, the function `(++)` has type `[a] -> [a] -> [a]`.
It takes in two lists, whose elements must be of the same type, but it does
not matter what that type is; `(++)` will simply append the lists regardless.

## Ad-Hoc Polymorphism

If parametric polymorphism requires us to do the same thing regardless of how
the type variables get instantiated, ad-hoc polymorphism lets us use the same
function symbol for a variety of different definitions in a way that is,
appropriately, pretty ad-hoc.

For example, suppose we want to fix `f` above by adding cases for when the
input isn't a `Bool`.

```Haskell
f a1 a2 = case (typeOf a1) of
    Int  -> a1 + a2
    Bool -> a1 && a2
    _    -> a1
```

where `f` exhibits specific behaviors for a specific set of types, and then has
some default behavior for the rest. We can easily implement this in
Java using `instanceof`, but being able to check a type at runtime like this
violates what we said in the last section about Haskell. Firstly, we (mostly)
don't have type annotations to check at runtime. Secondly, allowing functions
like this violates the guarantees of parametricity described above. In Haskell,
if we see a function of type `forall a, a -> a`, we know it must do the same
thing on all its inputs, i.e., it must be `id`. If we allowed functions like
`f` above, we would not have these guarantees. Parametricity is part of the
reason that just looking at the type of a Haskell function can tell you so much
about the function!

At the same time, we want to be able to overload symbols. It would be tedious
to have to use a different addition symbol for `Int`, `Integer`, `Double`, etc.
How can we do this kind of ad-hoc polymorphism in Haskell?

## Type classes

Let's look at the type of `(+)` in GHCi:

```Haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

Notice the `Num a =>` at the front. Let's look at a few others:

```Haskell
(==) :: Eq a => a -> a -> Bool
(<) :: Ord a => a -> a -> Bool
show :: Show a => a -> String
```

`Num`, `Eq`, `Ord`, and `Show` are type classes! We sometimes say that
functions like `(+)`, `(==)`, `(<)`, and `show` are "type class polymorphic."
Intuitively, type classes correspond to sets of types which have certain
function defined for them, and type class polymorphic functions work only
for types which are instances of the type class(es) in question.

As an example, let’s look in detail at the `Eq` type class.

```Haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

We can read this as follows: `Eq` is a type class with a single parameter `a`.
Any specific type `a` which wants to be an instance of `Eq` must have two
functions defined for it, `(==)` and `(/=)`, with the indicated type
signatures. For example, if we define a custom set type, `MySet`, we have to
supply two functions, each of which takes two `MySet`s and returns a `Bool`.
(We will see the syntax for this below.)

Consider a function like `elem`, which can be implemented as follows:

```Haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys
```

The `Eq a` that comes before the `=>` is a _type class constraint_. Having it
there allows us to use `(==)` in the body of the function, because if `a` is an
instance of `Eq`, we know `(==)` is defined. It is a type error to call the
function `elem` for a list of some type `a` which is not an instance of `Eq`,
since we wouldn't know what to do for `(==)`.

If a "normal" polymorphic type is a promise that the function will work for
whatever type the caller chooses, a type class polymorphic function is a
_restricted_ promise that the function will work for any type the caller
chooses, as long as the chosen type is an instance of the required type
class(es). Moreover, it can (and typically will!) have different behavior
depending on what that type is.

An important thing to note is that when `(==)` (or any type class method) is
used, the compiler figures out which implementation of `(==)` should be chosen,
based on the inferred types of its arguments.

To get a better handle on how this works in practice, let’s make our own type
and declare an instance of `Eq` for it.

```Haskell
data Foo = A Int | B Char

instance Eq Foo where
  (A i1) == (A i2) = i1 == i2
  (B c1) == (B c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)
```

It’s a bit annoying that we have to define both `(==)` and `(/=)`. In fact,
type classes can give default implementations of methods in terms of other
methods, which will be used whenever an instance does not override the
default definition with its own. In fact, the `Eq` class is actually declared
like this:

```Haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

This means that when we make an instance of `Eq`, we can define either `(==)`
or `(/=)`, whichever is more convenient; the other one will be automatically
defined in terms of the one we specify. This is nice, because it preserves the
conceptual idea that these two functions are opposites, which is otherwise not
enforced. (However, we have to be careful: if we don’t specify either one, we
get infinite recursion!)

As it turns out, `Eq` (along with a few other standard type classes) is
special: GHC is able to automatically generate instances of `Eq` for us.

```Haskell
data Foo = A Int | B Char
  deriving (Eq, Ord, Show)
```

This tells GHC to automatically derive instances of the `Eq`, `Ord`, and `Show`
type classes for our data type `Foo`.

## vs. Java interfaces

Type classes are quite similar to Java interfaces. Both define a set of
types/classes which implement a specified list of operations. However, there
are a couple of important ways in which type classes are more general than Java
interfaces:

1. When a Java class is defined, any interfaces it implements must be declared.
Type class instances, on the other hand, are declared separately from the
declaration of the corresponding types, and can even be put in a separate
module.

2. The types that can be specified for type class methods are more general and
flexible than the signatures that can be given for Java interface methods,
especially when multi-parameter type classes enter the picture. For example,
consider a hypothetical type class:

    ```Haskell
    class Something a b where
      something :: a -> b -> Bool
    ```

    Which implementation of `something` the compiler should choose depends on
    both the types `a` and `b`. There is no easy way to do this in Java.
    Haskell type classes can also easily handle multi-argument methods, as in:

    ```Haskell
    class Num a where
      (+) :: a -> a -> a
      ...
    ```

    There is no nice way to do this in Java: for one thing, one of the two
    arguments would have to be the "privileged" one which is actually getting
    the `(+)` method invoked on it, and this asymmetry is awkward.

## Standard type classes

-   `Ord` is for types whose elements can be totally ordered, that is, where
  any two elements can be compared to see which is less than the other (or if
  they're equal). It provides comparison operations like `(<)` and `(<=)`, and
  also the `compare` function.

-   `Num` is for "numeric" types, which support things like addition,
  subtraction, and multipication. One thing to note is that integer literals
  are actually type class polymorphic:

    ```Haskell
    Prelude> :t 5
    5 :: Num a => a
    ```

    This means that literals like `5` can be used as `Ints`, `Doubles`, or any
    other type which is an instance of `Num`.

-   `Show` defines the method `show`, which is used to convert values into
  `String`s.

- We will talk about some other very commonly used typeclasses (`Monad`,
  `Functor`, and `Foldable`) in later lectures.

## Type class dependencies

Some type classes require dependencies on others. For example, the definition
of `Ord` looks like this:

```Haskell
class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>), (>=) :: a -> a -> Bool
    max, min             :: a -> a -> a

    compare x y = if x == y then EQ
                  else if x <= y then LT
                  else GT
    x <= y = case compare x y of { GT -> False; _ -> True }
    x >= y = y <= x
    x > y = not (x <= y)
    x < y = not (y <= x)
    max x y = if x <= y then y else x
    min x y = if x <= y then x else y
```

Aside: Notice how this provides a default implementation of `(<=)` in terms of
`compare`, and default implementations of every other function in terms of
`(<=)`. The minimum a user needs to provide is an implementation of either
`compare` or `(<=)`.

In this example, we need `==` to be defined in order to provide the default
implementation of `compare`. Conceptually, this makes sense: if a type has a
notion of `<=`, it should also have a notion of `==`. We can read
`(Eq a) => Ord a` as saying that nothing can be an instance of `Ord` unless it
also an instance of `Eq`.