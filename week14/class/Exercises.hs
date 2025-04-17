module Exercises where

import GHC.Natural (Natural)
import Text.Parsec
  ( between,
    char,
    digit,
    many1,
    parse,
    spaces,
    (<|>),
  )
import Text.Parsec.String (Parser)

-- Hint: the imports show some functions you will likely find useful

-- Goal: be able to parse expressions with parens, digits, optional spaces
-- and `+` into an Exp

data Exp
  = Num Natural
  | Add Exp Exp
  deriving (Show)

-- parse a sequence of digits
digitsP :: Parser String
digitsP = error "unimplemented"

-- parse input as a Num
-- for this class, AFTER you've parsed the string into a string of
-- all digits, you may use `readNat` to coerce it to a Natural
numP :: Parser Exp
numP = error "unimplemented"

-- parse two sub-expressions with `+` between them
addP :: Parser Exp
addP = error "unimplemented"

-- parse balanced parens and apply the input parser between them
-- Use `between`
parenP :: Parser a -> Parser a
parenP = error "unimplemented"

-- parse either an addition expression with parens around it, or a Num
expP :: Parser Exp
expP = error "unimplemented"

-- Use the following to use GHCi as a mini calculator!
-- Make sure to test an expression with nested expressions and an invalid
-- input (this should show an error message instead of crashing entirely)

-- Note: re-run main each time you want to test a new input
-- if you do not rerun `main`, GHCi will use its own parser
main :: IO ()
main = do
  input <- getLine
  case parse expP "" input of
    Left err -> print err
    Right exp -> print (eval exp)

eval :: Exp -> Natural
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2

-- some examples from lecture

chars :: Char -> Parser String
chars = many1 . char

abcs :: Parser String
abcs = do
  xsA <- chars 'a'
  xsB <- chars 'b'
  xsC <- chars 'c'
  return (xsA ++ xsB ++ xsC)

bracketedX :: Parser Char
bracketedX = between (char '[') (char ']') (char 'x')

digitInAbcs :: Parser Char
digitInAbcs = between abcs abcs digit

xOry :: Parser Char
xOry = char 'x' <|> char 'y'

-- unsafe version for simplicity
readNat :: String -> Natural
readNat = read
