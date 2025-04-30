module Monads.Error where

import           Control.Monad.Except

play :: Either String Int
play = do
  Left "boom" >>= \x -> return (x + 1)
  Right 100 >>= \x -> Left "No"
