module Parser where

import Control.Applicative

import NanoParser
import Syntax
import Lexer

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = do
    spaces
    string x
    spaces
    pure f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
      int
  <|> parens expr

shell :: Parser Expr
shell = do
    char '!'
    i <- identifier
    pure $ Shell i

run :: String -> Expr
run = runParser expr
