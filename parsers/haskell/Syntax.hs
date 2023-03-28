module Syntax where

import Data.Char
import Text.Printf

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

data HttpMethod
    = GET
    | POST
    | PUT
    | DELETE
    deriving Show

instance PrintfArg HttpMethod where
    formatArg x = formatArg $ show x

type Path = String

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  | Shell String
  | HttpEndpoint HttpMethod Path
  | NoOp
  deriving Show

data WebShell = WebShell
    { code :: [String]
    } deriving Show

eval :: Expr -> String
eval ex = case ex of
  Add a b -> eval a ++ "+" ++ eval b
  Mul a b -> eval a ++ "*" ++ eval b
  Sub a b -> eval a ++ "-" ++ eval b
  Lit n   -> show n
  HttpEndpoint method path -> printf "@app.%s(\"%s\")\ndef tmp(): pass" (fmap toLower $ show method) path
  Shell s -> "subprocess.Popen(['bash', '-c', r\"\"\""
      ++ s
      ++ "\"\"\"])"
  NoOp -> "\n"
