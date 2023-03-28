module Transpiler where

import System.IO
import Control.Monad (when, join)
import Data.List (intersperse, foldl')

import Syntax
import WebShellParser

transpile :: String -> WebShell
transpile expr = WebShell
    { code = fmap eval $ run expr
    }

save :: String -> IO ()
save input = do
    template <- readFile "template.py"
    writeFile "output.py" $ join $ intersperse "\n" $ contents $ lines template

    where
        webshell = transpile input
        contents template = foldr replaceTemplateLines [] template
        replaceTemplateLines line lines =
            case line of
              "# %CODE%" -> lines ++ code webshell
              _ -> line:lines
