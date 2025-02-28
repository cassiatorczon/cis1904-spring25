# Homework 6: Type Classes

**Due**: Wednesday, March 5 at 11:59 p.m.

## Overview

We will work with polynomials in this homework.

In our representation, we will avoid explicitly specifying the degrees of terms
by representing a polynomial as a list of coefficients, each of which has
degree equal to its position in the list.

```Haskell
newtype Poly = P [Int]
```

In this representation, the polynomial `3 + 5x + x^2` would be written as
`P [3, 5, 1]`. Make sure you fully understand this representation before moving
on to the exercises!

**Note**: You may use functions from `Data.List`. As a reminder, you may search
Hoogle to find these, but you may NOT use other search engines, ChatGPT, or
other online sources to find functions or solutions.

## Exercise 1

In this exercise, you will write an instance of `Eq` for `Poly`. The
automatically derived implementation would _not_ work:

```Haskell
instance Eq Poly where
  (==) :: Poly -> Poly -> Bool
  (P c1) == (P c2) = c1 == c2
```

There are situations where two polynomials are equivalent, but their list
representations are not. In particular, consider `P [1, 2, 0]` versus
`P [1, 2]`.

Implement the `(==)` function in a way that takes into account this subtlety
with zero coefficients. You will need to first erase the `deriving (Eq)`
underneath the definition of `Poly` and then implement your own `instance`.

## Exercise 2

In this exercise, you will write an instance of `Show` for `Poly`. For example,
`P [1, 2, 3]` should be displayed as `1 + 2x + 3x^2`.

It should satisfy these constraints:

-   Terms are displayed as `cx^d` where `c` is the coefficient and `d` is the
    degree.
    + If `d` is 0, then only the coefficient is displayed.
    + If `d` is 1, then the format is simply `cx`.
    + If `c` is 0, omit the term (unless there are no nonzero terms; see the
        third bullet point).

-   Terms are separated by the `+` sign with a single space on each side of it.

-   As a special case, for a polynomial with no nonzero coefficients, display
    `0` rather than the empty string.

We break this problem down into several steps.

1.  Implement a helper function `showTerm`, which, given a coefficient (the
    first agument) and an exponent (the second argument), generates the string
    for that term. For example, `showTerm 3 2` should evaluate to `"3x^2"`.
2.  Use `showTerm` and an appropriate list function to combine the list of
    coefficients `cs` and the (implicit) list of degrees. (Hint: recall
    `[0..]`, which can be used with `Natural`.) In the above example, you
    should get the list `["1", "2x", "3x^2"]`.
3.  Remove the strings corresponding to terms with coefficient 0.
4.  Make the list into a single string by inserting `" + "` between each
    element. Find a function in the `Data.List` library to help with this.
    It's a bit tricky to find — it will help to recall that a `String` is
    just a `[Char]`.
5.  Handle the special case where there are no nonzero coefficients.

## Exercise 3

What is a number? In Haskell, a number is any type that is an instance of the
`Num` typeclass. Polynomials can be added, multiplied, and so on, just like any
other number. We will write an instance of `Num` for `Poly` over the course of
the remaining exercises.

First, implement `negate`, which should negate all of the coefficients of the
polynomial. For example, `P [1, 2, 3]` becomes `P [-1, -2, -3]`.

Second, implement `fromInteger`, which converts an integer into a degree zero
polynomial. For example, `3` becomes `P [3]`.

Remember that `negate` and `fromInteger` are functions that you can use on
any type that is an instance of `Num` type class, including for the `Int`
coefficients in the list.

## Exercise 4

Next, we define polynomial addition. We need to add the coefficients pointwise
for each term in the two polynomials.

For example, `(5 + x) + (1 + x + 3x^2) = 6 + 2x + 3x^2`. In terms of our
representation, this means that `P [5, 1] + P [1, 1, 3] = P [6, 2, 3]`.

## Exercise 5

Finally, we define polynomial multiplication. To multiply two polynomials, each
term in the first polynomial must be multiplied by each term in the second
polynomial. The easiest way to achieve this is to build up a `[Poly]` where each
element is the polynomial resulting from multiplying a single coefficient in the
first polynomial by each coefficient in the second polynomial. (You will likely
want a helper function for this so you can work with the underlying integer
lists directly.)

Since the terms do not explicitly state their exponents, you will have to shift
the output before multiplying it by each consecutive coefficient. For example
`P [1, 1, 1] * P [2, 2]` will yield the list
`[P [2, 2], P [0, 2, 2], P [0, 0, 2, 2]]`. You can then simply `sum` this list,
since we've already defined `(+)`.

## Conclusion

Now that we've finished implementing the `Num` instance, we can write and
manipulate polynomials very similarly to how we would in mathematical notation.
See `final` for an example. Note that though we did not implement `^` and `-`
explicitly, these are automatically defined for us in terms of the functions we
did define.
