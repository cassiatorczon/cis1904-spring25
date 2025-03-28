# Homework 7: IO

**Due**: Wednesday, April 2 at 11:59 p.m.

## Exercise 1

Take a look
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:29).
Observe that `FilePath` is just a _type synonym_ for `String`. They're entirely
interchangeable, but we use a new name for documentation purposes and for
maintainability, in case we choose to change the implementation later.

Try using `readFile` and `writeFile` a bit. Figure out if the following
statements are true or false, and fill in the corresponding variable
in `Exercises.hs`.

1. `readFile` will throw an exception if given the name of a file that does
   not exist.

2. `writeFile` will throw an exception if given the name of a file that does
   not exist.

3. `writeFile` will overwrite the contents of an existing file.

## Exercise 2

Practice using `readFile` and `writeFile` by implementing the short functions
below. Do **not** use any recursive functions in your solutions to this
exercise.

### (a)

`lengthFile` should return the number of characters (including whitespace)
in the contents of the input file.

### (b)

`concatFiles` takes as parameters a list of input files and an output file. It
should concatenate the contents of the input files and write those contents
into the output file.

You should use `mapM`, which for our purposes has type
`(a -> IO b) -> [a] -> IO [b]`.

## Exercise 3

In this exercise, you will reimplement some `IO` functions for printing.
Note that `putChar :: Char -> IO ()` takes a character and prints it.

1. (Re)implement `putStr` in terms of `putChar`.
   Do it twice: first with a recursive function (`putStr'`), then with `mapM_`
   (`putStr''`), which for our purposes has type `(a -> IO b) -> [a] -> IO ()`.

2. (Re)implement `putStrLn` in terms of `putStr'`.

3. (Re)implement `print` in terms of `putStrLn'`.

If you are not sure how some of these functions work, refer to the documentation
[here](https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#g:27)
or try them out in GHCi.

To test your reimplementations, make sure you get the same output on these
commands in GHCi:

```
ghci> putStr' "hello, world!"
hello, world!ghci> putStrLn' "hello again"
hello again
ghci> print' 3
3
```

## Grading

In addition to the usual style considerations, please make sure to use `do`
notation.