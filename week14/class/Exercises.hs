module Exercises where

import GHC.Natural (Natural)
import Text.Parsec
  ( between,
    char,
    digit,
    many1,
    parse,
    satisfy,
    spaces,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

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
-- for this class, after you've parsed the string into a string of
-- all digits, you may use `readNat` to coerce it to a Natural
numP :: Parser Exp
numP = error "unimplemented"

-- parse two sub-expressions and `+` between them
addP :: Parser Exp
addP = error "unimplemented"

-- parse balanced parens and apply the input parser between them
parenP :: Parser a -> Parser a
parenP = error "unimplemented"

-- parse either an addition expression with parens around it, or a Num
expP :: Parser Exp
expP = error "unimplemented"

-- Use the following to use GHCi as a mini calculator!

eval :: Exp -> Natural
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2

main :: IO ()
main = do
  input <- getLine
  case parse expP "" input of
    Left err -> print err
    Right exp -> print (eval exp)

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
