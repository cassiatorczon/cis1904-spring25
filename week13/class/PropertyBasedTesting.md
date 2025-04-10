
# Property-Based Testing
So far we have tested our code using ordinary unit testing. This week we will
learn how to use a powerful alternative: property-based testing.

## Motivation

Suppose we want to merge two sorted lists.

```Haskell
-- | Assuming the input lists are sorted, combine the lists into a
-- sorted output.
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted (x:xs) (y:ys)
  | x < y     = x : mergeSorted xs ys
  | otherwise = y : mergeSorted xs ys
mergeSorted _ _ = []
```

How can we check that this works as expected? We could write a unit test, i.e.,
we could write out a few examples of expected outputs for given inputs to the
function, choosing those inputs and calculating the expected outputs ourselves.
This is a very common testing style, but it has a few problems.
First, we have to do the work of coming up with examples and calculating their
expected outputs ourselves. We might easily make a mistake in the second part,
invalidating our test. Our testing is nowhere near exhaustive, and it is
usually heavily biased: humans are very prone to forgetting to test whole
classes of potential inputs, such as negative inputs to functions with integer
arguments. The whole process is tedious and not very thorough. Can we do
better?

## Property-based testing

With wholemeal programming, we try to reason about the entire program at once,
to generalize when possible, and to focus on the core ideas of our code.
With that in mind, what does it actually mean to merge two sorted lists into
a sorted output?

In this case, we want two _properties_: the output list should contain all
the elements of the input lists, with the right multiplicities, and it should
be sorted. What does "sorted" mean? Each element of the list should be less
than or equal to the next.

The key difference here is that we are not defining correctness in terms of a
couple of examples that may not completely capture the behavior of the code.
We are defining correctness in terms of _properties_ that should hold for every
output, regardless of input. We can then get the computer to generate _many_
pseudo-random valid inputs for us and, rather than manually supplying expected
outputs for each generated input, we can just check that the output for each
input satisfies the given properties. Instead of testing our code on three
cases, we can test it on thousands, without having to write each case
ourselves.

Most languages used in industry, including Python, Java, and C, provide
standard libraries for property-based testing. Haskell's is called QuickCheck.
Let's look at an example.

```Haskell
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_elements :: [Int] -> [Int] -> Bool
prop_elements xs ys = sameElements (mergeSorted xs ys) (xs ++ ys)
```

`sameElements` checks whether two lists have the same elements with the
same multiplicities. (`(\\)` takes the difference of two lists, i.e., it
removes each element of the second from the first.)

`prop_elements` then says that `mergeSorted xs ys` should have the same elements
as `xs` and `ys` combined. (By convention, property names begin with `prop_`
and follow camel case after that.) Let’s test it!

(Because QuickCheck generates inputs pseudorandomly, your results may look
slightly different.)

```
*Main> quickCheck prop_elements
*** Failed! Falsifiable (after 5 tests and 4 shrinks):
[]
[0]
```

The first thing we can see here is that our function is wrong, but
works on at least one example --- QuickCheck got through one passing test case
before discovering a failing test case. QuickCheck then shows us the failing
test case: the first argument was `[]`, and the second was `[0]`. We can see
the output of `mergeSorted` on these inputs ourselves in GHCi: running
`merge [] [0]` gives us `[]`, not the correct output `[0]`.

Notice that, out of all the possible randomly generated counterexamples,
QuickCheck shows one that is small and readable. In part this is because the
distribution of potential inputs favors small-to-medium lists, but it is mostly
because QuickCheck runs a function called `shrink` on all counterexamples.
After it finds a test case that causes failure, it tries to "shrink" the inputs
(i.e., find related but smaller inputs) to find the locally minimal version
that still fails so it can report that one. This is a large part of what makes
QuickCheck so useful: if it didn't shrink counterexamples, they would tend to
be unwieldy and hard to reason about.

As an example, suppose QuickCheck initially found the counterexample
`[-5, -5] []`. `[]` is already as simple as possible, but `[-5, -5]` can be
shrunk. There are two ways to do this: by shortening the list, or by
simplifying the elements. `shrink` starts with the first. There are two
possible proper sublists of `[-5, -5]`: `[]` and `[-5]`. QuickCheck tries `[]`
(keeping `[]` from before as the second argument), but that passes, so it
cannot be given as a counterexample. QuickCheck then tries `[-5]`, again
keeping `[]` as the second argument. This fails, so it is a valid
counterexample! Can it be shrunk further? The only sublist of `[-5]` is `[]`,
which we already saw passes, so the only way to simplify it is by simplifying
`-5`. Positive numbers are often easier for humans to reason about than
negative numbers, so QuickCheck next tries `[5]`. This also fails. Finally,
QuickCheck shrinks `[5]` to `[0]`, which also fails!
Can we shrink it further? The only list QuickCheck considers "smaller"
than `[0]` is `[]`, and we already saw that that is not a counterexample, so
we are done, and `[0] []` gets returned as our counterexample.

(If you want to observe all the test cases QuickCheck tries, including during
shrinking, use `verboseCheck` instead of `quickCheck`.)

One note about the property in this example: the type signature tells us that
the property takes lists of integers, not any type `a`. This is because when
generating, QuickCheck needs a concrete type for the elements of the list so it
can generate them, and if the property does not specify that type, QuickCheck
will usually pick the "simplest" type it knows about, `()`. This often does not
make for very good test cases, so we specify that the elements should be `Int`s
to avoid that. Keep this in mind when writing any property for polymorphic
functions.

## Implications

Let's fix the implementation of `mergeSorted`:

```Haskell
-- | Assuming the input lists are sorted, combine the lists into a
-- sorted output.
mergeSorted' :: Ord a => [a] -> [a] -> [a]
mergeSorted' (x:xs) (y:ys)
  | x < y     = x : mergeSorted' xs (y:ys)
  | otherwise = y : mergeSorted' (x:xs) ys
mergeSorted' [] ys = ys
mergeSorted' xs [] = xs

prop_elements' :: [Int] -> [Int] -> Bool
prop_elements' xs ys = sameElements (mergeSorted' xs ys) (xs ++ ys)
```

```
*Main> quickCheck prop_elements'
+++ OK, passed 100 tests.
```

Recall that we said `mergeSorted` was correct if its output had the same elements as
its inputs combined and was sorted. We test the second part of that with
another property:

```Haskell
isSorted :: (Ord a) => [a] -> Bool
isSorted (x1 : x2 : xs) = x1 <= x2 && isSorted (x2 : xs)
isSorted _ = True

prop_sorted :: [Int] -> [Int] -> Bool
prop_sorted xs ys = isSorted (mergeSorted' xs ys)
```

```
*Main> quickCheck prop_sorted
*** Failed! Falsifiable (after 4 tests and 3 shrinks):
[]
[1,0]
```

What happened? In the comment above our implementation of `mergeSorted'`, we said
that we were assuming the inputs to `mergeSorted'` are be sorted. QuickCheck,
however, does not know that, so it generated the test case `[] [1,0]`, which
failed. We can fix this by adding an implication to our property:

```Haskell
prop_sorted' :: [Int] -> [Int] -> Property
prop_sorted' xs ys = isSorted xs && isSorted ys ==> isSorted (mergeSorted' xs ys)
```

The operator `(==>)` has type `Testable prop => Bool -> prop -> Property`. It
takes a `Bool` and an element of some `Testable` type and produces a
`Property`. Note that the return type is no longer `Bool`; we will come back to
this later. In this example, we can think of `(==>)` as telling QuickCheck to
discard any generated inputs that do not meet the conditions specified to the
left of `==>`.

Let’s test it:

```
*Main> quickCheck prop_sorted'
*** Gave up! Passed only 22 tests; 1000 discarded tests.
```

There aren’t any failing tests, but there aren’t a lot of succeeding tests
either. This is because QuickCheck will run a test only when both
randomly-generated input lists are in sorted order. The odds that a
randomly-generated list of length n is sorted is 1/n!, and we need two sorted
lists. Most of the randomly generated test cases are going to get discarded.

## Arbitrary

How does QuickCheck generate the arbitrary test cases, anyway? It uses the
`Arbitrary` typeclass:

```Haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink    :: a -> [a]
```

`Arbitrary` is the typeclass for types we know how to generate, defined by
`arbitrary`, which is a `Gen a` (i.e., a generator for the type `a`), and
`shrink`, whose behavior we described above. Any type that is an instance of
this class has an associated definition for `arbitrary` and `shrink`. The
standard library includes an `Arbitrary` instance for lists already, but its
definition of `arbitrary` doesn't guarantee that generated lists are sorted.
(Due to parametricity, it actually _can't_ guarantee this, but also
conceptually we do not always want list inputs to be sorted. That restriction
is specific to the property we are testing here.)
Luckily, this is a common enough problem that QuickCheck offers
a solution in the form of `OrderedList`, a wrapper around `List` with its own
`Arbitrary` instance declaration that restricts `arbitrary` to only produce
sorted lists:

```Haskell
newtype OrderedList a = Ordered { getOrdered :: [a] }
instance (Ord a, Arbitrary a) => Arbitrary (OrderedList a) where ...
```

Now, let’s rewrite our property:

```Haskell
prop_sorted :: OrderedList Int -> OrderedList Int -> Bool
prop_sorted (Ordered xs) (Ordered ys) = isSorted (mergeSorted' xs ys)
```

```
*Main> quickCheck prop_sorted
+++ OK, passed 100 tests.
```

Let’s look more in depth at the types. (Full instance declaration
details omitted here for brevity.)

```Haskell
quickCheck :: Testable prop => prop -> IO ()

class Testable prop where
  property :: prop -> Property

instance Testable Bool where ...

instance Testable Property where ...

instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop) where ...
```

We can run `quickCheck` on anything that’s `Testable`. `Bool` is an instance of
`Testable`, as is `Property`. The last instance declaration about even says
that any function is `Testable` as long as its argument can be generated
(i.e., the argument type is an instance of `Arbitrary`),
its argument can be converted to a String for display when a
failure is discovered (i.e., the argument type is an instance of `Show`), and
its result type is an instance of `Testable`.

Is `[Int] -> [Int] -> Bool` `Testable`? Let's look at the instance declaration
above. Recall that `[Int] -> [Int] -> Bool` is equivalent to
`[Int] -> ([Int] -> Bool)`. Let's look at the last instance declaration above.
`[Int]` is an instance of `Arbitrary` and `Show`, and `Bool` is an instance of
`Testable`, so `([Int] -> Bool)` is an instance of `Testable. By the same
logic, then, since the input type is an instance of `Arbitrary` and `Show` and
the output type is an instance of `Testable`, we know
`[Int] -> ([Int] -> Bool)` is `Testable`.

The actual implementation of `quickCheck` is quite complicated, but for our
purposes, the main thing to note is that `quickCheck` can take as an argument
a function whose input type is an instance of `Arbitrary`, and, under the hood,
call `arbitrary` to generate test input values for that function. That’s how
changing the argument types to `OrderedList Int` got us the result we wanted;
`quickCheck` was then using a different definition for `arbitrary`.

## Generating arbitrary data

When you want to use QuickCheck with your own datatypes, you must write an
`Arbitrary` instance for them. Here, we’ll discuss how to do so.

Let’s make a data structure identical to `List` except for the names.

```Haskell
data AlmostList a = Empty | NonEmpty a (AlmostList a)

toList :: AlmostList a -> [a]
toList Empty          = []
toList (NonEmpty x xs) = x : toList xs

instance Show a => AlmostList a where
  show = show . toList
```

If we want an `Arbitrary` instance, we must define the `arbitrary` method, of
type `Gen (AlmostList a)`. Naturally, if we want to generate an almost-list
of `a`s, we will need to know how to generate `a`s, so we include the
restriction `Arbitrary a`:

```Haskell
instance Arbitrary a => Arbitrary (AlmostList a) where
  arbitrary = genList         -- defined below
  ```

How should we define this? First, note that QuickCheck provides combinators
to help create generators in the
[“Generator combinators” section](https://hackage.haskell.org/package/QuickCheck-2.15.0.1/docs/Test-QuickCheck.html#g:9) of the documentation.

Next, think about how you, as a human, would generate an arbitrary list. One
way to do it is to choose an arbitrary length (say, between 0 and 10), and then
choose each element arbitrarily. Here is an implementation of that strategy:

```Haskell
fromList :: [a] -> AlmostList a
fromList [] = Empty
fromList (x : xs) = NonEmpty x (fromList xs)

genList :: Arbitrary a => Gen (AlmostList a)
genList = do
  len <- choose (0, 10)
  vals <- replicateM len arbitrary
  return $ fromList vals
```

`Gen`, it turns out, is a monad, so we can use `do` notation. This is not
I/O, but you can think of it somewhat similarly: instead of reading from
terminal, we are randomly generating a value and then using `<-` to give
that value a name. In this example, `choose` randomly picks a number from
0 to 10. `replicateM` then chooses `len` values of type `a` using the
definition of `arbitrary` for `a`. Finally, we convert this list of
`len` generated values to an `AlmsotList`. This whole definition is a
recipe for generating something of type `AlmostList a`, so its type is
`Gen (AlmostList a)`, with the restriction that `a` must itself be
an instance of `Arbitrary`.

Let’s try it out:

```
*Main> sample genList
[(),(),(),(),(),()]
[]
[(),(),(),(),(),(),(),(),()]
[(),(),(),(),(),()]
[(),(),(),(),(),(),(),(),()]
[()]
[(),(),(),(),(),(),(),()]
[(),(),(),(),(),(),(),(),()]
[(),(),()]
[(),(),(),(),(),()]
[(),(),(),(),(),(),(),(),(),()]
```

The arbitrary lengths are working, but the element generation is
uninterestsing. Let’s use a type annotation to  override GHC’s default choice
of ()!

```
*Main> sample (genList :: Gen (AlmostList Int))
[0,0,0,0,0,0,0,0,0,0]
[]
[-2,3,1,0,4,-1]
[-5,0,2,1,-1,-3]
[-5,-6,-7,-2,-8,7,-3,4,-6]
[4,-3,-3,2,-9,9]
[]
[10,-1]
[9,-7,-16,3,15]
[0,14,-1,0]
[3,18,-13,-17,-20,-8]
```

That’s better, but this generation still isn’t great, because it only generates
lists of length at most 10. What if a function over lists only fails on large
inputs? We’d like the length of the generated values to be unbounded. Here’s
one way to do that:

```Haskell
genList :: Arbitrary a => Gen (AlmostList a)
genList = do
  stop_now <- arbitrary             -- randomly generate a Bool
  if stop_now
     then return Empty
     else do
       x <- arbitrary               -- randomly generate an `a`
       xs <- genList
       return (NonEmpty x xs)
```

Here we recursively build up a generator for `AlmostList a`, randomly deciding
at each recursive call whether we should stop or keep generating more elements.

```
*Main> sample (genList :: Gen (AlmostList Int))
[0,0,0,0,0,0]
[]
[3,-3]
[]
[]
[-1,-1]
[-10]
[]
[]
[11]
[-20,-14]
```

These lengths are unbounded, but we’re getting a lot of empty lists. This is
because at every recursive call, there’s a 50% chance of producing Empty. That
means that a list of length n will appear in expectation only one out of 2n
tries. In other words, lengths are unbounded, but most lengths are very unlikely.

The way to make progress here is to use the `sized` combinator. QuickCheck is
set up to try “simple” arbitrary things before “complex” arbitrary things, to
make counterexamples easier to understand. The way it does this is using a size
parameter, internal to the `Gen` monad. The more generating QuickCheck does,
the higher this parameter gets, and the bigger the generated values are likely
to get. We want to use the size parameter to do our generation.

Let’s look at the type of `sized`:

```Haskell
sized :: (Int -> Gen a) -> Gen a
```

Essentially, we give `sized` a function that will produce a generator for
any given input size, and it gives us a generator using a randomly chosen
size.

```Haskell
genList :: Arbitrary a => Gen (AlmostList a)
genList = sized $ \size -> do
  len <- choose (0, size)
  vals <- replicateM len arbitrary
  return $ fromList vals
```

```
*Main> sample (genList :: Gen (AlmostList Int))
[]
[-2]
[-1,3,4]
[-4,-2,1,-1]
[]
[]
[12,3,11,0,3,-12,10,5,11,12]
[-4,-8,-9,2,14,5,8,11,-1,7,11,-8,2,-6]
[6,10,-5,15,6]
[-3,-18,-4]
[9,19,13,-19]
```

That worked nicely – the lists tend to get longer the later they appear. The
idea is that `sized` takes in a function saying what to do with the size
parameter it generates. We just use a lambda function as the one argument to
`sized`, where the lambda binds the size parameter, and then we can refer to
that size internally.

As one more example, we can also choose arbitrary generators from a list, with
weights to determine frequency. Although the length-choosing strategy of
`genList` works well for lists, the following technique is much better for,
say, trees, although here we show it with lists:

```Haskell
genList :: Arbitrary a => Gen (AlmostList a)
genList = sized $ \size -> do
  frequency [ (1, return Empty)
            , (size, do x <- arbitrary
                        xs <- resize (size - 1) genList
                        return (NonEmpty x xs) )]
```
```
*Main> sample (genList :: Gen (AlmostList Int))
[]
[2,0]
[4,0,-1]
[-6,-2,3,-1,-1]
[6,6]
[2,2,2,6,6]
[-6,-9,5,-5]
[8,7,5,7,7,-1,-2,-1,-5,-3]
[15,-12,14,13,-5,-10,-9,-8,-2]
[12,-11,-8,6,-6,-4,11,11]
[-7,1,-3,4,-3,-9,4,6,-2,10,-9,-7,5,7,1]
```

Let’s look at the type of `frequency`:

```Haskell
frequency :: [(Int, Gen a)] -> Gen a
```

It takes a list of (Int, Gen a) pairs and produces a Gen a. The numbers in the
list give the likelihood of choosing that element (as an integer that will be
compared to the sum across all elements). Above, we fixed the
frequency of `Empty` at 1, but let the likelihood of `NonEmpty` vary according
to the desired size. Then, in the recursive call to `genList`, we used `resize`
to lower the size parameter. Otherwise, it’s too likely to get a runaway list
that gets unworkably large or never terminates.

## Model-based testing
Our example with merging sorted lists focused on the key properties that
define what it means to merge two sorted lists correctly. What if we
actually already have a correct implementation of `mergeSorted`, and we just
want to test a new implementation (say, an optimized one) against that
correct version?

This is an example of _model-based testing_, where we test one version of
a function against another. For example, the standard library actually
already has a function called `merge` with the same functionality as
`mergeSorted`. We could have written a model-based test for it as
follows:

```Haskell
prop_merge :: [Int] -> [Int] -> Bool
prop_merge xs ys = mergeSorted xs ys == merge xs ys
```

It is important to make sure these tests are not circular. If you want
to use model-based testing, make sure your reference implementation
definitely is correct.

## Conclusion

Property based testing is an amazingly effective approach to testing. It works
especially well in Haskell because of the combination of purity, which means
that one only needs to vary the arguments to a function, and the type system,
which allows us to use type classes to very generically and create modular
to generate random input. QuickCheck came out in 1999 and has since then
inspired similar libraries in most languages that are communly used in
industry.