module Main where

import System.IO
import Control.Applicative
import Control.Monad

import Syntax
import Parser
import Transpiler

main :: IO ()
main = do
    save "!hello"
    pure ()
