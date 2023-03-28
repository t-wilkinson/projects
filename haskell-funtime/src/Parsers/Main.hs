{-# Language
  FlexibleContexts
#-}

module Parsers.Main where

-- import Text.Parsec
-- import Text.Parsec.Combinator
-- import Text.Parsec.Char

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Control.Monad.Combinators
import Data.Void (Void)
import Data.Functor.Identity (Identity)

type Parser = ParsecT Custom String Identity String

data Custom
    = Trivial String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
    showErrorComponent (Trivial s) = s

parseEmailAddress :: Parser
parseEmailAddress = do
    c <- withRecovery (\x -> pure "error") $ (:[]) <$> satisfy (`elem` "ab")
    d <- many (oneOf "zasf")
    pure $ c <> d
    -- case invalid of
    --   Left _ -> pure (0, "")
    --   Right x -> pure x
    -- r <- observing p
    -- c <- case r of
    -- case
    --   Left _ -> pure (0, "")
    --   Right x -> pure x
    -- d <- takeWhileP (Just "h") (`elem` "hi")



-- parseEmailAddress
--     :: (Stream s m Char)
--     => ParsecT s u m String
-- parseEmailAddress = do
--     addr <- manyTill (noneOf ";") (char '@' <?> "hey")
--     domain <- manyTill (noneOf ";") (char '.' <?> "wanting a .")
--     tld <- manyTill (noneOf ";") eof
--     pure $ addr <> "@" <> domain <> "." <> tld
