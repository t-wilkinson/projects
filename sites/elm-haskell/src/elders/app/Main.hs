module Main where

import           Elders.Api                     ( app )
import           Elders.Database                ( openDatabase )
import           Network.Wai.Handler.Warp       ( run )


main :: IO ()
main = do
  database <- openDatabase
  run 8000 (app database)
