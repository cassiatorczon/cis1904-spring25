# Lazy Evaluation

Laziness is an _evaluation strategy_: it is a way of deciding when (or if)
subexpressions of a program get evaluated. There are many evaluation
strategies, but they largely fall into two buckets, _strict evaluation_ and
_non-strict evaluation_.

Laziness is an example of the second. The central idea of lazy evaluation is
that we do not evaluate function arguments (and remember, Haskell programs are
built out of composed functions) until their results are needed. To understand
what this means, we will first look at the more familar strict evaluation.

## Strict evaluation

Under a strict evaluation strategy, function arguments are completely evaluated
before they are passed to the function. For example, suppose we have defined

```Haskell
f x y = x + 2
```

and now wish to evaluate `f 5 (29^35792)`. A strict language will first
completely evaluate `5` (already done) and `29^35792` (which is a lot of work)
before passing the results to `f`. In examples such as this one, this is
wasteful, since `f` ignores its second argument, so all the work to compute
`29^35792` was unnecessary. (It might seem silly to have an argument that never
gets used, but in more complex code, especially with branching, there may be
arguments that get used in some cases but not others.)

Strict evaluation is much more common than lazy evaluation. (Many popular
languages use a strict evaluation strategy called _call-by-sharing_, a variant
of the strict _call-by-value_ strategy. Haskell's lazy strategy is called
_call-by-need_, a variant of _call-by-name_.) If strict evaluation sometimes
does unnecessary computation, why do language designers choose it?

The benefit of strict evaluation is that it is easy to predict when and in what
order things will happen. Usually languages with strict evaluation will even
specify the order in which function arguments should be evaluated (e.g. from
left to right).

For example, in Java, if we write

```Java
f (print_status(), increment_counter())
```

we know that the status will be printed, and then the counter will be
incremented, and then the results of doing those things will be passed to `f`,
regardless of whether `f` actually ends up using those results.

If the printing of the status and incrementing of the counter could
independently happen, or not, in either order, depending on whether f happens
to use their results, it would be extremely confusing. When such “side effects”
are allowed, strict evaluation is much easier to reason about.

## Side effects and purity

By “side effect”, we mean anything that causes evaluation
of an expression to interact with something outside itself. A term evaluating
(e.g., `4 + 4` simplifying to `8`) is "pure"; if anything occurs beyond that
simplification (e.g., printing to the terminal), that is a "side effect".

The presence or absence of side effects significantly affects the choice of
evaluation strategy. Such outside interactions are time-sensitive; for example:

* Modifying a global variable may affect the evaluation of other expressions
* Print statements may need to be in a certain order with respect to each other
* Data read from a file may affect the result of an expression

In general, these are the same sorts of issues you might encounter with
concurrency!

Historically, this is the reason Haskell is pure: initially, its designers
just wanted to make a lazy functional language, but they quickly
realized it would be extremely difficult to reason about unless it was
specifically a _pure_ lazy functional language, i.e., one without side effects.

However, a language with no side effects at all would not be very useful. The
only thing you could do with such a language would be to load up your programs
in an interpreter and evaluate expressions (like when you type a Haskell
expression into the GHCi REPL). You
would not be able to get any input from the user, or print anything to the
screen, or read from a file. The challenge facing the Haskell designers was to
come up with a way to allow such effects in a principled, restricted way that
did not interfere with the essential purity of the language. They finally did
come up with something (namely, monads) which we’ll talk about in a later
lecture.

## Pattern matching drives evaluation

Under a lazy evaluation strategy, evaluation of function arguments is delayed
for as long as possible. When some expression is given as an
argument to a function, Haskell simply packages that argument in a metaphorical
box, called a "thunk", without evaluating it. If that argument is later needed,
Haskell unboxes it (i.e., "forces the thunk") and evaluates it at that time.

For example, when evaluating `f 5 (29^35792)`, the second argument will simply be
packaged up into a thunk without doing any actual computation, and `f` will be
called immediately. Since `f` never uses its second argument, the thunk will just
be thrown away by the garbage collector.

When is it “necessary” to evaluate an expression?

We call the "amount" of output required from a program the _demand_ on the
program. The demand depends on context -- for example, if we evaluate a
term directly in GHCi, it demands the full output. (When we talk about
evaluating terms in this document, you can imagine we're doing so in GHCi.)
That is, if we type in `1 + 1`, GHCi will demand that `+` gets evaluated, and
the term reduces to `2`. If we typed in `f 5 (29^35792)`, as above, it would
demand the output of `f`; `f` just does not then demand output from its second
argument. Haskell's runtime normally demands the output of `main`, which may
then demand output from other functions, which may demand output from their
arguments, and so on. In general, how do we determine the demand of a function
on its arguments?

The examples above focused on whether a function used its arguments, but this
is actually not the most important distinction. Consider the following:

```Haskell
f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]
```

`f1` and `f2` both use their argument -- but there is still a big difference
between them. Although `f1` uses its argument `m`, it does not need to know
anything about it. `m` can remain completely unevaluated, and the unevaluated
expression is simply put in a list. Put another way, the result of `f1 e` does
not depend on the value of `e`, or the constructors used to build it.

`f2`, on the other hand, needs to know something about its argument in order to
proceed: was it constructed with `Nothing` or `Just`? That is, in order to
evaluate `f2 e`, we must first evaluate `e` at least enough to know what case
we're in.

The slogan to remember is “pattern matching drives evaluation”.

Note that even with `f2`, we do not need to evaluate `e` all the way! We do not,
for example, need to know what `x` is. We do not pattern match on `x` itself,
so any thunks containing its value remain unforced.

As another example, consider the following function that gets the head of a list.
```Haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h : _) = h
```

Suppose we want to evaluate `safeHead ([1 + 5] ++ [2 * 3])`. To see how many
thunks there are, let's write it out more fully.

`safeHead (append (Cons (add 1 5) Nil) (Cons (mult 2 3) Nil))`

Note: to make it all prefix form so we can easily see where the arguments
are, we're writing `append` for `(++)`, `add` for `+`, and `mult` for `*`.
`+` and `*` are primitive (i.e., built-in) functions, but their arguments still
get thunked.

In total, all arguments to `safeHead`, `append`, `Cons`, `add`, and `mult` are
individually thunked! Note that this means we have thunks within thunks.
Let's look at the sequence of evaluation steps for this term.

```Haskell
safeHead (append (Cons (add 1 5) Nil) (Cons (mult 2 3) Nil))
```
`safeHead` has to pattern match, so it forces the outermost thunk on
`append (Cons (add 1 5) Nil) (Cons (mult 2 3) Nil)`.

`append` also has to pattern match, so it forces the outermost thunk on
`Cons (add 1 5) Nil`.

`Cons` does not need to pattern match, so we don't force any more thunks yet.

```Haskell
safeHead (Cons (add 1 5) (append Nil (Cons (mult 2 3) Nil)))
```

Now `safeHead` knows the shape of its argument, so it can pattern match.

```Haskell
add 1 5
```

`add` needs both its arguments to be fully evaluated, so it forces the thunks
on both of them. Luckily, they're already fully simplified (integers are a
primitive type). We then get our final answer:

```Haskell
6
```

Let’s do another example: we’ll evaluate
`take 3 (repeat 7)`. For reference, here are the definitions of `repeat` and
`take`:

```Haskell
repeat :: a -> [a]
repeat x = x : repeat x

take :: Int -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs
```

Carrying out the evaluation step-by-step looks something like this:

```Haskell
  take 3 (repeat 7)
```
`3 <= 0` reduces to `False`, so we proceed to the second clause, which
    needs to match on the second argument. So we must expand
    `repeat 7` one step.

```Haskell
take 3 (7 : repeat 7)
```
The second clause does not match, but the third clause does. Note that `3-1`
does not get evaluated yet!

```Haskell
7 : take (3-1) (repeat 7)
```
In order to decide on the first clause, we must test `(3-1) <= 0`, which
requires evaluating `3-1`.

```Haskell
7 : take 2 (repeat 7)
```
`2 <= 0` reduces to `False`, so we must expand `repeat 7` again.

```Haskell
7 : take 2 (7 : repeat 7)
```
The rest is similar.

```Haskell
7 : 7 : take (2-1) (repeat 7)
7 : 7 : take 1 (repeat 7)
7 : 7 : take 1 (7 : repeat 7)
7 : 7 : 7 : take (1-1) (repeat 7)
7 : 7 : 7 : take 0 (repeat 7)
7 : 7 : 7 : []
```

Note that although evaluation could be implemented exactly like the above,
most Haskell compilers will do something a bit smarter. In particular, GHC uses
a technique called graph reduction, where the expression being evaluated is
actually represented as a graph, so that different parts of the expression
can share pointers to the same subexpression. This ensures that work is not
duplicated unnecessarily. For example, if `f x = x * x`, evaluating `f (1+1)`
will only do one addition, because the subexpression `1+1` will be shared
between the two occurrences of `x`. (This is what makes Haskell's evaluation
strategy _call-by-need_, a specific type of non-strict evaluation strategy.)

## Consequences

Laziness has some very interesting, pervasive, and subtle consequences, many of
which stem from it being difficult to reason about. Let’s explore a few of
them.

### Purity

As we’ve already seen, choosing a lazy evaluation strategy essentially forces
language designers to also choose purity.

### Understanding space usage

It sometimes becomes tricky to reason about the space usage of programs.
Let's go back to the definition of foldl:

```Haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
```

Consider how evaluation proceeds when we evaluate `foldl (+) 0 [1,2,3]`:

```Haskell
  foldl (+) 0 [1,2,3]
= foldl (+) (0+1) [2,3]
= foldl (+) ((0+1)+2) [3]
= foldl (+) (((0+1)+2)+3) []
= (((0+1)+2)+3)
= ((1+2)+3)
= (3+3)
= 6
```

Since the value of the accumulator is not demanded until we've recursed through
the entire list, the accumulator simply builds up a big unevaluated expression
`(((0+1)+2)+3)`, which finally gets reduced to a value at the end. There are at
least two problems with this. One is that it’s simply inefficient: there’s no
point in transferring all the numbers from the list into a different list-like
thing (the accumulator thunk) before actually adding them up. The second
problem is more subtle: evaluating the expression `(((0+1)+2)+3)` actually
requires pushing the `3` and `2` onto a stack before being able to compute
`0+1` and then unwinding the stack, adding along the way. This is not a problem
for this small example, but for very long lists this can lead to a stack
overflow.

The solution in this case is to use the `foldl'` function instead of `foldl`,
which adds a bit of strictness: in particular, `foldl'` requires its second
argument (the accumulator) to be evaluated before it proceeds, so a large thunk
never builds up:

```Haskell
  foldl' (+) 0 [1,2,3]
= foldl' (+) (0+1) [2,3]
= foldl' (+) 1 [2,3]
= foldl' (+) (1+2) [3]
= foldl' (+) 3 [3]
= foldl' (+) (3+3) []
= foldl' (+) 6 []
= 6
```

As you can see, `foldl'` does the additions along the way, which is what we
really want. In general, Haskell gives us an operator, `!`, for forcing
strictness in cases like this.

(If you’re interested in learning about how `foldl'` achieves this, you can
read about `seq` on the Haskell wiki.)

### Short-circuiting operators

In some languages (Java, C++) the boolean operators `&&` and `||` (logical AND
and OR) are short-circuiting: for example, if the first argument to `&&`
evaluates to false, the whole expression will immediately evaluate to false
without touching the second argument. However, this behavior has to be wired
into the Java and C++ language standards as a special case. Normally, in a
strict langauge, both arguments of a two-argument function are be evaluated
before calling the function. So the short-circuiting behavior of `&&` and `||`
is a special exception to the usual strict semantics of the language.

In Haskell, however, we can define short-circuiting operators without any
special cases. In fact, `(&&)` and `(||)` are just regular library functions!
For example, here’s how `(&&)` is defined:

```Haskell
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False
```

Notice how this definition of `(&&)` does not pattern-match on its second
argument. In fact, if the first argument is `False`, the second argument is
entirely ignored. Since `(&&)` does not pattern-match on its second argument at
all, it is short-circuiting in exactly the same way as the `&&` operator in
Java or C++.

Notice that `(&&)` also could have been defined like this:

```Haskell
(&&!) :: Bool -> Bool -> Bool
True  &&! True  = True
True  &&! False = False
False &&! True  = False
False &&! False = False
```

While this version takes on the same values as `(&&)`, it uses the `!` operator
mentioned above, so it does not short circuit. For example, consider:

```Haskell
False &&  (34^9784346 > 34987345)
False &&! (34^9784346 > 34987345)
```

These will both evaluate to `False`, but the second one will take a lot longer!
Another example:

```Haskell
False &&  (head [] == 'x')
False &&! (head [] == 'x')
```

The first one is again `False`, whereas the second one will crash. Try it!

All of this points out that there are some interesting issues surrounding
laziness to be considered when defining a function.

### User-defined control structures

Taking the idea of short-circuiting operators one step further, in Haskell we
can define our own control structures.

Most languages have some sort of special built-in `if` construct because, like
short-circuiting Boolean operators, `if` has special behavior. Based on the
value of the test, it evaluates only one of the two branches. It would defeat
the whole purpose if both branches were evaluated every time!

In Haskell, however, we can define `if` as a library function!

```Haskell
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y
```

That said, `if` doesn’t get used that much in Haskell; in most situations we
prefer pattern-matching or guards.

We can also define other control structures, as we'll see with monads.

### Infinite data structures

Lazy evaluation also means that we can work with infinite data structures. In
fact, we’ve already seen a few examples, such as `repeat 7`, which represents
an infinite list containing nothing but `7`. Defining an infinite data
structure actually only creates a thunk, which we can think of as a “seed” out
of which the entire data structure can potentially grow, depending on what
parts actually are needed.

Another practical application area is extremely large data structures,
such as the trees that might arise as the state space of a game like go or
chess. Although the tree is finite in theory, it is so large that it would
likely not fit in memory. Using Haskell, we can define the tree of all possible
moves, and then write a separate algorithm to explore the tree in whatever way
we want. Only the parts of the tree which are actually explored will be
computed.

### Pipelining/wholemeal programming

We mentioned at the start of the semester that doing “pipelined” incremental
transformations of a large data structure can be more memory-efficient. Now we
can see why: due to laziness, each stage of the pipeline only generates each
bit of the result as it is demanded by the next stage in the pipeline. Their
demand on the input is, to some degree, synchronized.

### Dynamic programming

As a more specific example of what lazy evaluation buys us, consider the
technique of dynamic programming. Usually, one must take great care to fill
in entries of a dynamic programming table in the proper order, so that every
time we compute the value of a cell, its dependencies have already been
computed. If we get the order wrong, we get invalid results.

However, using lazy evaluation, we can get the Haskell runtime to work out the
proper order of evaluation for us!
