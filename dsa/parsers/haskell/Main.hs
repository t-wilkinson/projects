module Main where

import System.IO
import Control.Applicative
import Control.Monad

import Syntax
import WebShellParser
import Transpiler

main :: IO ()
main = do
    contents <- readFile "examples/input.webshell"
    save contents

    pure ()
