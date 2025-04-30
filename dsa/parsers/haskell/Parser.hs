module Parser where

import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)]}

instance Functor Parser where
    fmap f p = Parser $ \s -> [ (f a, s') | (a, s') <- parse p s ]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser pf) <*> (Parser pa) = Parser $ \s -> [ (f a, s') | (f, fs) <- pf s, (a, s') <- pa fs ]

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ p s

-- Encodes computations with multiple paths, with potential failure
-- A sort of union
instance Alternative Parser where
    empty = failure
    (Parser p1) <|> (Parser p2) = Parser $ \s ->
        case p1 s of
          [] -> p2 s
          res -> res

instance MonadPlus Parser where
  mzero = empty
  mplus (Parser p1) (Parser p2) = Parser $ \s -> p1 s ++ p2 s

failure = Parser $ \_ -> []

runParser :: Show a => Parser a -> String -> a
runParser p s = case parse p s of
                  [(a, [])] -> a
                  [(a', s')] -> error ("Did not finish parsing: " ++ s')
                  _ -> error "Too many parses left."

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s ->
    case s of
      [] -> empty
      (c:cs) -> if f c then [(c, cs)] else []

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a
