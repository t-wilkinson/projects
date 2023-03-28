{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Monads.ST where

import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.IO.Class ( liftIO )


newtype ST s a = ST
    { unsafeRunST :: a
    }

instance Functor (ST s) where
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
    pure = ST
    ST f <*> ST a = ST $ seq a $ f a

instance Monad (ST s) where
    ST a >>= f = seq a $ f a

newtype STRef s a = STRef
    { unSTRef :: IORef a
    }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref $ f a

runST :: (forall s. ST s a) -> a
runST = unsafeRunST

main :: IO ()
main = print $ runST test

-- test :: String
-- test = runST $ newSTRef "heelo" >>= readSTRef
test :: ST s String
test = do
    ref <- newSTRef "hello"
    readSTRef ref

