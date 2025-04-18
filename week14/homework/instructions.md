# Homework 11: Real(ish) World Haskell

Submit this file (`instructions.md`) with the answers filled in below to
Gradescope for manual grading.

**Due**: Wednesday, April 23 at 11:59 p.m.

## Exercise 1: Project

As you've likely noticed, there are no other files in this folder this week.
Throughout the semester, we've set up the assignments so that you only need to
fill in an `Exercises.hs` file. How do people set up Haskell projects in the
real world?

Run the following command inside of the `homework` folder in the terminal.

```
$ stack new myproject --bare
```

What are the names of the four Haskell files that were created? You may need to
look in the various subdirectories.

**FILL IN HERE**:

Let's dive deeper into the command we just ran. Stack is a _build sytem_ for
Haskell that we have been using all semester (think `stack ghci`). Feel free to
poke around [here](https://docs.haskellstack.org/en/stable/) if you're
interested in learning more.

`stack new` can be used to create a new Haskell project with a sensible default
structure, and we pass in the parameter `myproject` as the name of the project.

What does the `--bare` flag do? To find out, try running the same command
without this flag in a temporary directory **elsewhere**.
That is, make an empty folder within `week14`, open that folder in VSCode,
open terminal, and run `stack new myproject`.
Then write the difference below.

**FILL IN HERE**:

The rest of the problems should be done back in this original `homework`
directory, not in the temporary one.

Replace the contents of `stack.yaml` with the following lines.

```
resolver: lts-22.32
packages:
  - .
```

This sets the resolver, which implicitly sets
the GHC version to the same version that we've been using throughout
this class. Go to `Lib.hs` and click around a bit to check that you are
receiving the usual VSCode typing feedback before continuing. You may need to
first reload VSCode and wait a few seconds.

## Exercise 2: Modules

We will start in the `src` folder, where the implementation of whatever project
you are building will typically go. We will talk about `test` and `app` later.
(Note that in VSCode, you should still have the top-level `homework` directory
open, so that the editor can find the config files it needs.)

Haskell code is organized into modules. Create a new file `MyModule.hs` in
`src`, and start the file with the module definition `module MyModule where` as
the first line.

### 2a: Imports

Paste this snippet into the file:

```Haskell
foo :: Bool
foo = isJust (Just 3)
```

VSCode should immediately give you an error. What does the part of the error
message starting with "Variable" say?

**FILL IN HERE**:

As we've alluded to before, a certain number of useful functions are provided
in Haskell's `Prelude`, and these functions can be used from the get-go. Other
functions, such as `isJust`, need to be _imported_.

If, for example, we needed functions from `Data.Char` that are not in the
`Prelude`, we can either add this statement

```Haskell
import Data.Char
```

to import everyrthing in `Data.Char`, or a more precise statement such as

```Haskell
import Data.Char (toLower, toUpper)
```

to just import `toLower` and `toUpper`.

What is the import statement that will only import `isJust`?

**FILL IN HERE**:

Add that to your file after the module definition but before the rest of the
code. `foo` should now compile.

We can also import definitions from other modules within the same project. For
example, add `import Lib` — you should now be able to reference `someFunc`,
i.e., the following should compile in MyModule:

```Haskell
someFunc' :: IO ()
someFunc' = someFunc
```

### 2b: Exports

Modules can also _export_ functions that can then be used in other modules. By
default, if we just write `module Lib where`, everything defined in `Lib.hs`
will be exported. But this is not always desirable! Sometimes we want to
hide implementation details and just expose an interface.

We may instead have, for example, internal helper functions that we do not want
to make visible to others. If you look at the actual module definition of
`Lib.hs`, the syntax is instead along the lines of

```Haskell
module Lib (someFunc) where
```

This says that `Lib` will only export `someFunc` and no other definitions. Add
some `otherFunc` to `Lib` without changing the module definition. If you try to
reference it in `MyModule`, you should get an error — it wasn't exported. Then,
add the second function to the list of exports:

```Haskell
module Lib (someFunc, otherFunc) where
```

The error in `MyModule` should now go away.

### 2c: Exports, continued

Finally, add this snippet to `Lib`:

```Haskell
data Color = Red | Blue | Green
```

To export the type `Color`, we would add `Color` to the list of exports. To
export the constructors as well, we would need to either mention them
explicitly by instead adding `Color (Red, Blue, Green)` or more succinctly
`Color (..)`, where the `..` just means that we export all constructors.

Let's further examine the difference between these two exports. If you just
export the `Color` type but not the constructors, what is an example of a
function of type `Color -> Color` that we can write in `MyModule`?

**FILL IN HERE**:

If you export the `Color` type and its constructors, what is an example of a
function of type `Color -> Color` that we can write in `MyModule` that we could
_not_ in the first case?

**FILL IN HERE**:

## Exercise 3: Testing

We have seen both unit tests (using `HUnit`) and property-based tests (using
`QuickCheck`), which we have included in the exercises files and run in GHCi.
When we are testing a larger project, we may want to have a separate test suite
that we can easily run in bulk.

In this project, we have the `test` folder, with a `Spec.hs` file.
These names come from the `package.yaml` file, which has a `tests` section that
sets the testing directory to be `test` and the main file in it to be
`Spec.hs`.

### 3a: Dependencies

Let's use `QuickCheck`! Try writing `import Test.QuickCheck` at the top of
`Spec.hs`. You should get an error message.

Functions within the `Prelude` are automatically imported for use, and
libraries within Haskell's `base` package are automatically importable.
`QuickCheck` is not in `base`, so we need to include it as a package in our
project explicitly.

In `package.yaml`, add this line to the list of `dependencies` under `tests`:

```
- QuickCheck
```

Now, the error message in `Spec.hs` should go away. (You need'll to reload
VSCode. You also may still be warned that we are not currently using anything
in the `QuickCheck` module — we'll change that soon.)

Would this have worked if we just added `QuickCheck` to the dependencies
under `executables`, not `tests`? Try it, fill in below, and then revert to
having it just under `tests`. Again, you will need to reload VSCode.

**FILL IN HERE**:

In general, To figure out what a package's name is, you can check the `Hackage`
page: https://hackage.haskell.org/package/QuickCheck. It's the thing before the
colon.

### 3b: Tests

Copy the full definitions of `sort`, `prop_SortOrdered`, `ordered`,
and your answer to the first exercise from the property-based testing
in-class exercises, and paste them into
`test/Spec.hs`. (More realistically, we would want to import the functions we
want to test from the `src` folder, but this is easier for our purposes.)

Replace the main function with this (the first line is not a typo):

```Haskell
return []

main :: IO Bool
main = $quickCheckAll
```

You'll also need to add this language extension to the top of this file:
`{-# LANGUAGE TemplateHaskell #-}`. Normally, order of functions in a
Haskell file does not matter, but here it does, so make sure `main` (and the
`return []` before it) are at the bottom of your file.

That is, `QuickCheck` has this handy function `quickCheckAll`, which uses
TemplateHaskell to find all of the properties starting with `prop_` in the
file, and then it runs the `quickCheck` function on each property.

Instead of running the tests in GHCi like we're used to, let's run it directly
in the terminal using

```
$ stack test
```

It will compile the various files in the project and then execute whatever is
in the `main` function of `Spec.hs`. To demonstrate that you can run the tests,
paste the output _after_ `myproject> test (suite: myproject-test)` below.

**FILL IN HERE**:

## Exercise 4: Executable

We also might want to run our code itself outside of GHCi — that is, we should
be able to compile a Haskell project into an executable that we can run in the
command line (or, perhaps, as part of a larger, non-Haskell-exclusive project).

Our `package.yaml` is set up so that the top-level program that the exceutable
will run is the `main` function in `app/Main.hs`.

Take a quick look at that file, though don't be alarmed if you get some error
message — VSCode is not very good at correctly compiling the files in `app`
before the project has been built. As long as you have not removed the
`someFunc` function in `src/Lib.hs`, you should be fine.

To build the executable, run

```
$ stack build
```

To execute the executable, which is called `myproject-exe`, run

```
$ stack exec myproject-exe
```

What is the output (ignoring the `stack` warnings you may receive)?

**FILL IN HERE**:

## Conclusion

Of course, the Haskell project in this homework is just a series of toy
examples, but we now have the basic tools at our disposal to build more complex
projects.

## Grading

This homework will be manually graded out of 20 points.
Submit just this file to Gradescope.
