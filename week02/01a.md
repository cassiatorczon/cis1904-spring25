# Haskell Basics, Part 1

## What is Haskell?

This semester, we will explore the unique characteristics of the Haskell language
and how they impact the process of Haskell programming.

**Functional**

When we say that Haskell is a _functional_ language, we usually
have in mind two things:

-   Functions are _first-class_. That is, functions are values which can
    be used in exactly the same ways as any other sort of value.

-   The meaning of Haskell programs is centered around evaluating
    expressions rather than executing instructions.

**Pure**

Haskell expressions are always _pure_:

-   No mutation! Everything (variables, data structures...) is immutable.

-   Expressions do not have "side effects" such as updating global variables.

-   Calling the same function with the same arguments results in
    the same output every time.

Yes, this takes some getting used to, but the benefit is that our programs will
be easier to debug, maintain, and reason about.

**Lazy**

In Haskell, expressions are not evaluated until their results are needed.
Laziness...

-   Enables a more compositional programming style.

-   Makes it possible to work with infinite data structures.

-   Leads to complications when reasoning about time and space usage.

**Statically typed**

Every Haskell expression has a type, and types are checked at
compile-time. Programs with type errors will not compile, much
less run.

## Themes

Throughout this course, we will focus on three main themes.

**Types**

Haskell's highly expressive type system:

-   _Helps clarify thinking and express program structure._

    Often, when designing a new chunk of code, we will find it
    helpful to first write down the types.

-   _Serves as a form of documentation._

    A function's type communicates a lot about what the function
    might do and how it can be used, even without written documentation.

-   _Turns run-time errors into compile-time errors._

    Being able to catch a large class of errors early provides
    for a smoother, safer programming experience.

**Abstraction**

"Don't Repeat Yourself" is a common programming mantra. Taking similar pieces of code
and factoring out their commonality is known as _abstraction_. We will gradually see
more and more language features that allow Haskell to facilitate abstraction.

**Wholemeal programming**

A quote from Ralf Hinze:

> "Functional languages excel at wholemeal programming, a term coined by
> Geraint Jones. Wholemeal programming means to think big: work with an
> entire list, rather than a sequence of elements; develop a solution
> space, rather than an individual solution; imagine a graph, rather
> than a single path. The wholemeal approach often offers new insights
> or provides new perspectives on a given problem."

For example, consider this pseudocode in a Java-ish sort of language:

```Java
int acc = 0;
for ( int i = 0; i < lst.length; i++ ) {
    acc = acc + 3 * lst[i];
}
```

This code concerns itself with the details of iterating over an array by
keeping track of an index. It also mixes together what can
more usefully be thought of as two separate operations: multiplying
every item in a list by 3, and summing the results.

In Haskell, we can just write

```Haskell
sum (map (3 *) lst)
```
