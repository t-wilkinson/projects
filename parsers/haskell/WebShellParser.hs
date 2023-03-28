module WebShellParser where

import Control.Applicative

import Parser
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

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
      int
  <|> parens expr

shell :: Parser Expr
shell = do
    char '!'
    command <- some $ notchar '\n'
    pure $ Shell command

httpMethod :: Parser HttpMethod
httpMethod = do
    method <- (string "GET" *> pure GET) <|> (string "POST" *> pure POST)
    return method

httpPath :: Parser String
httpPath = do
    char '/'
    path <- some $ satisfy $ \x -> not $ elem x " \t\n\r"
    return $ "/" ++ path

httpEndpoint :: Parser Expr
httpEndpoint = do
    method <- httpMethod
    spaces
    path <- httpPath
    pure $ HttpEndpoint method path

expr :: Parser Expr
expr =
        term `chainl1` addop
    <|> shell
    <|> httpEndpoint

file :: Parser [Expr]
file = many (expr <|> fmap (\_ -> NoOp) newline)

run :: String -> [Expr]
run = runParser file

-- topLevel :: Parser [Expr]
-- topLevel = many $ do

