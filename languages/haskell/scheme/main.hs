import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (withArgs, getArgs)

main :: IO ()
main = withArgs ["hi", "bye"] $ do
    args <- getArgs
    print args
    pure ()

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
