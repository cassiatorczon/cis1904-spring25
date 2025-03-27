# IO

## The problem with purity

Remember that Haskell is a _pure_ language. This means two primary things:

1. Functions may not have any external effects. For example, a function may not
print anything on the screen. Functions may only compute their outputs.

2. Functions may not depend on external stuff. For example, they may not read
from the keyboard, or filesystem, or network. Functions may depend only on
their inputs — put another way, functions should give the same output for the
same input every time. (This is part of the idea of _referential transparency_,
a core tenet of Haskell.)

But sometimes we do want to be able to do stuff like this! In fact, these
things are possible in Haskell, but it looks very different than in most
other languages.

## The IO type

The solution is a special type constructor called `IO`. Values of type `IO a`
are _descriptions_ of effectful computations, which, if executed, would
(possibly) perform some effectful input/output operations and (eventually)
produce a value of type `a`.

There is a level of indirection here that’s crucial to understand. A value of
type `IO a`, in and of itself, is just an inert, perfectly safe thing with no
effects. It is just a description of an effectful computation. One way to think
of it is as a _first-class imperative program_. In other words, we can in
Haskell write a pure, functional program that builds an impure, imperative
program, which the runtime system may then execute.

As an illustration, suppose you have

```Haskell
c :: Cake
```

What do you have? A cake! Plain and simple. Suppose instead you have

```Haskell
r :: Recipe Cake
```

What do you have? A cake? No, you have some _instructions_ for how to make a
cake.

Not only do you not actually have a cake, merely being in possession of the
recipe has no effect on anything else whatsoever. Simply holding the recipe
in your hand does not cause your oven to get hot or flour to be spilled all
over your floor or anything of that sort. To actually produce a cake, the
recipe must be followed (causing the oven to get hot, the flour to spill, and
so on).

In the same way, a value of type `IO a` is just a "recipe" for producing a
value of type `a` (and possibly having some effects along the way). Like any
other value, it can be passed as an argument, returned as the output of a
function, stored in a data structure, or (as we will see shortly) combined with
other `IO` values into more complex recipes.

So, how do values of type `IO a` ever get executed? The Haskell
compiler looks for a special value

```Haskell
main :: IO ()
```

which will actually get handed to the runtime system and executed.

Why `()`? In some cases, we don't care about building a value. Think about a
TODO list. Some tasks have a resulting value.
"Get groceries" results in groceries, and it can also have side effects
(perhaps tiredness from the trip or carrying one's groceries).

Other tasks have side effects but no result values. For example, doing laundry
does not create new laundry, but it does have the side effect that the laundry
is now clean. Sending a letter does not result in a value, but it has the
effect that the letter is sent.

Let's look more closely at `IO a`. `IO` is a _type constructor_, like `List` or
`Maybe`. We must specify a type for the results of following an instruction,
but what if the instruction has no resulting value? In that case, we return
`()`. `()`, pronounced "unit", is the sole value of the type that is also
written `()` and pronounced "unit". In Haskell, this acts as a natural default
type and value for I/O.


Let’s write our first actual Haskell program with IO! We can use the function

```Haskell
putStrLn :: String -> IO ()
```

which, given a `String`, returns an `IO` computation that will (when executed)
print out that `String` on the screen. That is, `putStrLn` takes a `String` and
uses it to write a mini recipe for printing that string.

To tell the runtime system to follow that recipe, we put this in a file called
`Hello.hs`:

```Haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```
Then typing `runhaskell Hello.hs` at a command-line prompt results in our
message getting printed to the screen.
Alternatively, we can run `stack ghci Hello.hs` and then in GHCi run `main`.

## There is no `String` "inside" an `IO String`

Many new Haskell users end up at some point asking a question like "I have an
`IO String`, how do I turn it into a `String`?". There is no function to do
this. A value of type `IO String` is a description of some computation, a
recipe for generating a `String`. There is no `String` "inside" an `IO String`,
any more than there is a cake "inside" a cake recipe. To produce a `String` (or
a cake) requires actually executing the computation (or recipe).

## Combining IO

When writing a recipe for a pie, you may have a recipe for crust and a recipe
for filling, and you need to combine the two to get a recipe for the whole pie.

Similarly, we need a way to combine `IO` computations into larger ones.

The simplest way to combine two IO computations is with the `(>>)` operator
(pronounced "and then"), which has the type

```Haskell
(>>) :: IO a -> IO b -> IO b
```

This creates an `IO` computation which consists of running the two input
computations in sequence. Notice that the result of the first computation is
discarded; we only care about it for its effects. For example:

```Haskell
main :: IO ()
main = putStrLn "Hello" >> putStrLn "world!"
```

This works fine for code of the form “do this; do this; do this” where the
results don’t really matter. However, in general this is insufficient. What if
we don’t want to throw away the result from the first computation?

A first attempt at resolving the situation might be to have something of type
`IO a -> IO b -> IO (a, b)`. However, this is also insufficient. The reason is
that we want the second computation to be able to depend on the result of the
first.

For example, suppose we want to read an integer from the user and then print
out one more than the integer they entered. In this case the second computation
(printing some number on the screen) will be different depending on the result
of the first.

To do this, there is an operator `(>>=)` (pronounced "bind") with the type

```Haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

This can be difficult to wrap one’s head around at first! `(>>=)` takes a
computation which will produce a value of type `a`, and a function which gets
to build a second computation based on this intermediate value of type `a`.
The result of `(>>=)` is a (description of a) computation which performs the
first computation, uses its result to decide what to do next, and then does
that.

We can implement the example situation above. Note our use of
`readLn :: Read a => IO a` which is a computation that reads input from the
user and converts it into any type which is an instance of `Read`.

```Haskell
addOneAndPrint :: Int -> IO ()
addOneAndPrint n = print (n + 1)

main :: IO ().
main = putStr "Enter a number: " >> readLn >>= addOneAndPrint
```

Let's break this down.
`putStr "Enter a number: "` is a recipe for creating the unit value and along
the way printing "Enter a number: ".
`readLn` is a polymorphic recipe for creating something of type `a`, in this
case, an `Int`.
`addOneAndPrint` takes in an integer n, and returns a recipe for creating the
unit value and along the way printing n + 1.
`>>` combines the first two recipes into one recipe for printing "Enter a
number: " and then creating an `Int`.
`>>=` takes that recipe and the function `addOneAndPrint` and links them
together, creating one big recipe for printing "Enter a number: ", creating an
`Int` n, printing n + 1, and finally creating the unit value.

This can get a little convoluted, so Haskell provides `do` notation to make it
look like familiar imperative code. Note that this is just convenient notation;
under the hood, this is still functional code.

```Haskell
main :: IO ()
main = do
  putStr "Enter a number: "
  n <- readLn
  addOneAndPrint n
```

The first two lines have an implicit `>>` between them.
In the second line, everything to the right of `<-` is the next "step" of the
recipe.
`<-` is a bit like `>>=` and a lambda rolled into one. We take the value
created by everything to the right of `<-`, give
it the name to the left of `<-` and then we can refer to it with that name in
the following lines.
This looks very similar to a `let`, but there is an important difference!
`readLn` is _not_ a function that returns the
value that we bind to `n`, it is a step in the longer recipe being written
within the `do` block.
Think of a recipe for making a cake. Part of the recipe might tell you
to mix together salt, sugar, baking soda, and flour, and then a later part of
the recipe might refer to the resulting mixture as
"the dry ingredients" when giving further instructions. `n <- readLn` just
means that we will refer to the results of `readLn`
as `n` later in the recipe, but we are still just writing a recipe, not
following it yet.

Remember, this is still functional, and this fancy syntax desugars into
`main = putStr "Enter a number: " >> readLn >>= addOneAndPrint`.
This means we cannot put arbitrary expressions as lines of a `do` block,
because `>>` expects its arguments to have type `IO a` and `IO b` for some
`a` and `b`.