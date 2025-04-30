module Lexer where

import Control.Applicative
import Data.Char

import Parser

spaces :: Parser String
spaces = many $ oneOf " \t\n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a}

notchar :: Char -> Parser Char
notchar c = satisfy (c /=)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

reserved :: String -> Parser String
reserved s = token (string s)

digit :: Parser Char
digit = satisfy isDigit

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

identifier :: Parser String
identifier = some $ oneOf "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

newline :: Parser String
newline = some $ oneOf "\n"
