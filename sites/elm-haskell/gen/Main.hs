module Main where

import qualified Blog.Gen
import qualified Elders.Gen


main :: IO ()
main = do
    Blog.Gen.generate 
    Elders.Gen.generate

