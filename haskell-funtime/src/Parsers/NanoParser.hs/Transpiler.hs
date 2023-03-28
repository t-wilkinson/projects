module Transpiler where

import System.IO
import Control.Monad (when)

import Syntax
import Parser

transpile :: String -> String
transpile expr = eval $ run expr

save :: String -> IO ()
save input = do
    writeFile "file.py" contents
    where
        contents = transpile input
