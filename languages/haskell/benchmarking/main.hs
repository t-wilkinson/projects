{-# Language ApplicativeDo #-}
{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}

import Control.Monad
import Criterion.Main
import NeatInterpolation (text)
import Options.Applicative
import System.Environment (withArgs)
import Text.Printf (printf)
import Text.RawString.QQ (r)
import qualified BenchMarks as BM
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
    runCommandLine =<< execParser opts
    pure ()
  where
    opts = info (optsParser <**> helper)
        (fullDesc <> progDesc "Testing this parser." <> header "Header - a test.")

{- OPTS PARSER -}
runCommandLine :: OptsParser -> IO ()
runCommandLine OptsBase{..} = do
    putStrLn greeting
    putStrLn $ "The fib number of " <> show runFib <> " is " <> show (BM.fibInline runFib) <> "!"
    when runBench do
       withArgs [] myBench
runCommandLine OptsFile{..} = do
    putStrLn [r|I can do this: \asdf\fd \dfa" "asdf "\][] '|]
    let f = \a -> [text|the following is a var: $a|]
    Text.putStrLn $ f "one"
    printf "Will open %s and see what is inside.\n" runFile
    printf "Verbosity level is %d.\n" verbosity
runCommandLine OptsFiles{..} = do
    printf "%s\n" $ unlines runFiles
runCommandLine (OptsCommands com) = do
    putStrLn $ show com <> "!!!"

data OptsParser
    = OptsBase
        { runBench :: Bool
        , greeting :: String
        , runFib :: Int
        }
    | OptsFile
        { runFile :: String
        , verbosity :: Int
        }
    | OptsFiles
        { runFiles :: [String]
        }
    | OptsCommands Commands

data Commands
    = ComPost
    | ComGet
    deriving (Show)

optsParser,optsFilesParser,optsFileParser,optsBaseParser :: Parser OptsParser
optsParser =
    ( OptsCommands <$> subparser
        ( command "POST" (info (optsPostParser <**> helper) (progDesc "Make a POST request."))
       <> command "GET" (info optsGetParser (progDesc "Make a GET request."))
        )
    ) <|> optsBaseParser <|> optsFileParser <|> optsFilesParser

optsPostParser = pure ComPost
optsGetParser = pure ComPost

optsFilesParser = do
    runFiles <- some $ argument str $ metavar "FILENAMES"
    pure OptsFiles{..}

optsFileParser = do
    runFile <- strOption $ long "filename" <> metavar "FILENAME"
    verbosity <- length <$> many (flag' () (short 'v'))
    pure OptsFile{..}

optsBaseParser = do
    runBench <- switch
        $  long "bench"
        <> short 'b'
        <> help "Run the bench tests"
    greeting <- strOption
        $ long "greeting"
        <> metavar "GREETING"
        <> help "Get greeting."
    runFib <- option auto
        $ long "fib"
        <> help "Get the fibonocci sequence of <n>."
        <> showDefault
        <> value 10
        <> metavar "INT"
    pure OptsBase{..}

{- BENCHING -}
myBench :: IO ()
myBench = defaultMain
    [ bgroup "fibs"
        [ bench "inline" $ nf BM.fibInline 20
        , bench "no-inline" $ nf BM.fibNoInline 20
        ]
    ]
