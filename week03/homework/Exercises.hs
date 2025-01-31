module Exercises where

import Test.HUnit
  ( Test (..),
    assertBool,
    runTestTT,
    (~:),
    (~?=),
  )

{- Read [instructions.md] first. -}

-- Exercise 1:

data Nat
  = Z
  | S Nat
  deriving (Show, Eq)

add :: Nat -> Nat -> Nat
add = error "unimplemented"

exercise1 :: Test
exercise1 =
  "add"
    ~: [ add zero zero ~?= zero,
         add one two ~?= three,
         add two one ~?= three
       ]

-- Exercise 2:

data Exp
  = Num Nat
  | Add Exp Exp
  | Branch Exp Exp Exp
  deriving (Show)

eval :: Exp -> Nat
eval = error "unimplemented"

exercise2 :: Test
exercise2 =
  "eval"
    ~: [ eval (Num three) ~?= three,
         eval (Add (Num one) (Add (Num two) (Num three))) ~?= six,
         eval (Branch (Num zero) (Num one) (Num two)) ~?= one,
         eval (Branch (Num one) (Num one) (Num two)) ~?= two
       ]

-- Exercise 3:

data Instr
  = IPush Nat
  | IAdd
  | IBranch
  deriving (Show, Eq)

type Stack = [Nat]

exec1 :: Instr -> Stack -> Stack
exec1 = error "unimplemented"

exercise3 :: Test
exercise3 =
  "exec1"
    ~: [ exec1 (IPush one) [two] ~?= [one, two],
         exec1 IAdd [one, two, zero] ~?= [three, zero],
         exec1 IAdd [one] ~?= [one],
         exec1 IBranch [zero, one, two, three] ~?= [one, three],
         exec1 IBranch [one, one, two, three] ~?= [two, three]
       ]

-- Exercise 4:

exec :: [Instr] -> Stack -> Stack
exec = error "unimplemented"

exercise4 :: Test
exercise4 =
  "exec"
    ~: [ exec [] [one] ~?= [one],
         exec [IPush one, IPush two, IAdd, IPush three, IAdd] []
           ~?= [six]
       ]

-- Exercise 5:

compile :: Exp -> [Instr]
compile e = case e of
  Num n -> error "unimplemented"
  Add e1 e2 ->
    is2 ++ is1 ++ [IAdd]
    where
      is1 = compile e1
      is2 = compile e2
  Branch e1 e2 e3 -> error "unimplemented"

exercise5 :: Test
exercise5 =
  "compile"
    ~: [ exec (compile (Num three)) [] ~?= [three],
         exec (compile (Add (Num one) (Add (Num two) (Num three)))) [] ~?= [six],
         exec (compile (Branch (Num zero) (Num one) (Num two))) [] ~?= [one],
         exec (compile (Branch (Num one) (Num one) (Num two))) [] ~?= [two],
         exec
           ( compile
               (Branch (Add (Num one) (Num zero)) (Num zero) (Num three))
           )
           []
           ~?= [three]
       ]

{-
Write down the number of hours it took you to complete this homework. Please
also write one question you have about any of the material we have covered so
far, not necessarily from this week.
-}

time :: Double
time = error "unimplemented"

question :: String
question = error "unimplemented"

check :: Test
check =
  TestCase
    ( assertBool
        "fill in a time and question"
        ( time >= 0
            && question /= ""
        )
    )

main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3,
          exercise4,
          exercise5,
          check
        ]
  return ()

{-
These are just to make the tests easier to read; don't use them in your
solutions. Incidentally, this is an example of a very nice feature of Haskell:
definitions can appear after they are used!
-}

zero :: Nat
zero = Z

one :: Nat
one = S Z

two :: Nat
two = S (S Z)

three :: Nat
three = S (S (S Z))

six :: Nat
six = S (S (S (S (S (S Z)))))