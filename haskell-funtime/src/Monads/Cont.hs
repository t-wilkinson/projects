{-# Language
  RankNTypes
, GADTs
, KindSignatures
, DataKinds
, TypeOperators
, TypeFamilies
, UndecidableInstances
#-}

module Monads.Cont where

import Control.Monad.Cont
-- import Control.Monad.Trans.Cont
import Data.Kind (Type, Constraint)

data HasShow where
    HasShow :: Show t => t -> HasShow

instance Show HasShow where
    show (HasShow s) = show s

main :: IO ()
main = do
    -- traverse print ([HasShow test, HasShow test2] :: [HasShow])
    -- liftIO $ print $ runCont (cont (\f -> f "hi")) id
    -- runContT (test3 Error) target
    pure ()

target :: (MonadIO m) => Target -> m ()
target Success = liftIO $ print "Success"
target Error = liftIO $ print "failure"

data Target
    = Error
    | Success
    deriving (Show)

iden :: a ~ b => a -> b
iden a = a

test3 :: Target -> IO ()
test3 t = (`runContT` target) $ ContT $ \f -> do
    f t


twoC :: Cont r Int
twoC = pure 2

helloC :: Cont r String
helloC = pure "hello"

twoHelloC :: Cont r String
twoHelloC = do
    two <- twoC
    hello <- helloC
    return $ (show two) ++ hello

ex1 :: Cont r String
ex1 = do
    a <- cont (\f -> f 'a')
    b <- pure 5
    pure $ replicate b a
