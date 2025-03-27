module Examples where

---

ex1 :: IO ()
ex1 = putStr "Hello, " >> putStr "world!"

ex2 :: IO Int
ex2 = putStr "Enter a number: " >> readLn

ex3 :: IO ()
ex3 =
  putStr "Enter a number: "
    >> readLn
    >>= (\n -> print (n + 1))

ex4 :: IO Int
ex4 =
  getLine
    >>= (\input -> return (length input))

---

ex1' :: IO ()
ex1' = do
  putStr "Hello, "
  putStr "world!"

ex2' :: IO Int
ex2' = do
  putStr "Enter a number: "
  readLn

ex3' :: IO ()
ex3' = do
  putStr "Enter a number: "
  n <- readLn
  print (n + 1)

ex4' :: IO Int
ex4' = do
  input <- getLine
  return (length input)

ex5 :: IO ()
ex5 = do
  putStr "Give me five: "
  n <- readLn
  if n == 5
    then putStrLn "Hooray!"
    else do
      putStrLn "Try again."
      ex5