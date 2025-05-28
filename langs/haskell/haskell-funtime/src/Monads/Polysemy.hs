{-# Language
  DataKinds
, TemplateHaskell
, GADTs
, ScopedTypeVariables
, FlexibleContexts
, PolyKinds
, LambdaCase
, TypeOperators
#-}

module Monads.Polysemy where

-- import Polysemy.Error
-- import Polysemy.Input
-- import Polysemy.Output
-- import Polysemy.Reader
-- import Polysemy.State

import Polysemy
import System.Random (randomIO)

data Console m a where
    PrintLine :: String -> Console m ()
    ReadLine :: Console m String

data Random v m a where
    NextRandom :: Random v m v

makeSem ''Random
makeSem ''Console

infixl 9 &
(&) x f = f x

main :: IO ()
main = execute >>= putStrLn . show
  where
    execute = runM $ runRandomIO $ runConsoleIO $ program
    -- execute = program
    --     & runConsoleIO
    --     & runRandomIO
    --     & runM

program
    :: forall m r.
        ( Member Console r
        )
    => Member (Random Int) r
    => Sem r Int
program = do
    printLine "Insert number: "
    i1 <- readLine
    i2 <- nextRandom
    pure $ read i1 + i2

runConsoleIO
    :: Member (Embed IO) r
    => Sem (Console ': r) a -> Sem r a
runConsoleIO = interpret $ \case
    PrintLine line -> embed $ putStrLn line
    ReadLine -> embed $ getLine

runRandomIO
    :: Member (Embed IO) r
    => Sem (Random Int ': r) a -> Sem r a
runRandomIO = interpret $ \case
    NextRandom -> embed randomIO


