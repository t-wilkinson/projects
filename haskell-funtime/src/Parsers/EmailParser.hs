import Text.Parsec.Combinator
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Control.Monad.Combinators as M

type Parser a = M.Parsec Void String a
type ParserE a = M.Parsec Void String a

parseEmail :: Parser String
parseEmail = do
    M.space
    local     ← validChars ""
    offset    ← M.getOffset
    M.withRecovery (\err → failures offset ["Requires a '@'"]) $ void $ M.char '@'
    M.withRecovery (\err → failures offset ["Requs a '@'"]) $ void $ M.char '@'
    M.withRecovery (\err → failures offset ["Requirs a '@'"]) $ void $ M.char '@'
    domain    ← validChars "."
    offset    ← M.getOffset
    M.withRecovery (\err → failures offset ["Requires a '.'"]) $ void $ M.char '.'
    subdomain ← validChars ""
    M.space
    M.eof
    pure $ local <> "@" <> domain <> "." <> subdomain
    where
        failures ∷ Int → [String] → Parser ()
        failures offset fails = M.registerParseError $ M.FancyError offset $ Set.fromList $ fmap (M.ErrorFail$) fails
        validChars ∷ String → Parser String
        validChars others = M.some $ M.satisfy (`notElem` " =;<>[]{}@\"\\" <> others)

runParseEmail :: IO ()
runParseEmail = do
    let myparser = parseEmail *> M.getParserState <&> M.stateParseErrors
    let res = M.runParser myparser "" "asdf.com"
    case res of
        Left (M.ParseErrorBundle errs state) →
            let nontrivial (M.FancyError offset fancyErrs)
            errs <&>
        Right suc → print suc
    -- putStrLn $ either M.errorBundlePretty show $ M.runParser myParser "" "asdfasdf.com"
    -- print $ M.runParser myParser "" "asdfasdf.com"
    pure ()

