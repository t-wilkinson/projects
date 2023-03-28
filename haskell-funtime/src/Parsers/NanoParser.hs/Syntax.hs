module Syntax where

-------------------------------------------------------------------------------
-- Calulator parser
-------------------------------------------------------------------------------

-- number = [ "-" ] digit { digit }.
-- digit = "0" | "1" | ... | "8" | "9".
-- expr = term { addop term }.
-- term = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop = "+" | "-".
-- mulop = "*".

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  | Shell String
  deriving Show

eval :: Expr -> String
eval ex = case ex of
  Add a b -> eval a ++ "+" ++ eval b
  Mul a b -> eval a ++ "*" ++ eval b
  Sub a b -> eval a ++ "-" ++ eval b
  Lit n   -> show n
  Shell s -> show s

