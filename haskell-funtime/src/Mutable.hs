{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Mutable where

import Control.Monad.ST
import Data.STRef
import Data.IORef
import System.Timeout (timeout)
import Control.Concurrent
import Debug.Trace
import Text.Printf
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

main ∷ IO ()
main = do
    let section = printf "\n\n----------   %-6s ----------\n"
    section "ST"
    print myST

    section "IORef"
    ioref ← newIORef "bob"
    getMessage ioref
    print =<< readIORef ioref

--     let prints c = putChar c >> prints c
--     threadId ← forkIO (prints 'a')
--     timeout 100000 do
--         prints 'b'
--     killThread threadId

--     section "MVar"
--     mvar ← newEmptyMVar
--     forkIO do
--         v ← takeMVar mvar
--         print v
--         putMVar mvar 2
--     forkIO do
--         putMVar mvar 1

    section "STM"
    v ← atomically do
        tmvar ← newTMVar "stm"
        v ← readTMVar tmvar
        readTMVar tmvar
    print v

    pure ()

myST ∷ Int
myST = runST do
    stref ← newSTRef 0
    modifySTRef stref (+1)
    readSTRef stref

getMessage ∷ IORef String → IO ()
getMessage ioref = do
    modifyIORef ioref ("Hello " <>)
