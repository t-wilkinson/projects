{-# Language PartialTypeSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language DisambiguateRecordFields #-}

{- Type Level Progamming -}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language PolyKinds #-} -- implies KindSignatures
{-# Language RoleAnnotations #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}

{- Syntaxes -}
{-# Language TypeOperators #-}
{-# Language BangPatterns #-}
{-# Language BlockArguments #-}
{-# Language LambdaCase #-}
{-# Language MagicHash #-}
{-# Language ParallelListComp #-}
{-# Language PatternGuards #-}
{-# Language PatternSynonyms #-}
{-# Language RecursiveDo #-}
{-# Language ViewPatterns #-}
{-# Language DerivingVia #-} -- implies DerivingStrategies
{-# Language MultiWayIf #-}

{- Classes -}
{-# Language UndecidableSuperClasses #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fprint-equality-relations #-}

module Main where

import Control.Monad.Fix
import Control.Monad.ST
import Data.Coerce
import Data.IORef
import Data.STRef
import GHC.Exts
import GHC.Conc
import Text.Printf
import Data.Time (getCurrentTime)
import System.CPUTime
import Data.Foldable (foldl')
import GHC.Float (rationalToFloat, showFloat)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

import Data.Type.Equality
import Type.Reflection
import Data.Dynamic
import Data.Kind
-- import Data.Typeable

type T1 :: forall k. k -> Type
newtype T1 a = T1 Int deriving (Show, Eq)
type T2 :: forall k -> Type
newtype T2 a = T2 Int deriving (Show, Eq)

type role T1 nominal
type role T2 nominal

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
      []   -> return ()
      m:ms -> do
          putMVar children ms
          takeMVar m
          waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkFinally io (\_ -> putMVar mvar ())

main :: IO ()
main = do
    let avg = (\(sum,l) -> rationalToFloat sum l) . foldl' (\(!b, !c) a -> (a+b,c+1)) (0,0)
    forkChild $ do
        parallel <- (sequence $ replicate 1000 (time False)) >>= pure . avg
        notparallel <- (sequence $ replicate 1000 (time True)) >>= pure . avg
        printf "Parallel is %.3f times faster.\n" $ parallel / notparallel
    traceM "tracing"
    traverse putStrLn [
        show $ typeOf $ runST testSTRef
      , show $ (coerce (T1 8 :: T1 String) :: T2 Double)
      , show $ testEquality (typeOf 'a') (typeOf @Int 8)
      , show $ fromDyn (toDyn 'a') ('b')
      -- , show $ (unsafeCoerce# 'a'# :: Int#)
                      ]
    -- patterns
    linkedlist
    rectree
    waitForChildren
    pure ()

time :: Bool -> IO Integer
time parallel = do
    let x = foldr (+) 0 [1..1000000]
        y = foldr (*) 0 [1..1000000]
    !start <- getCPUTime
    let !z = if parallel then x`par`y else x`pseq`y
    !end <- getCPUTime
    pure $ end - start

-- (exists a. a) :: Top, universally uninhabited type which you can't do anything with
-- (forall a. a) :: Bottom, uninhabited but you can do anything you want with it

-- :k Int# :: TYPE 'IntRep
-- :t 0x85# :: Int#

data Test = Test String [Int] deriving (Show)
pattern Arrow t1 t2 = Test "->" [t1, t2]
pattern TailP xs <- x:xs
pattern HeadP x <- x:xs where
    HeadP x = [x]
data PosNeg = Pos Int | Neg Int deriving (Show)
pattern Smarter{ nonneg } <- Pos nonneg  where
  Smarter x | x >= 0    = (Pos x)
            | otherwise = (Neg x)

patterns :: IO ()
patterns = do
    print $ getArrow $ Arrow 1 2
    print $ getArrow $ Test ">" [1,2,3]
    print $ HeadP 8
    TailP xs <- pure [1..5]
    print xs
    print $ Smarter 8
    pure ()

getArrow :: Test -> Test
getArrow (Arrow t1 t2) = Arrow t1 t2
getArrow _ = Test "" []

data Node = Node Int (IORef Node)

linkedlist :: IO ()
linkedlist = do
    p <- mknode
    Node x q <- readIORef p
    print x
    Node y _ <- readIORef q
    print y

mknode :: IO (IORef Node)
mknode = mfix \p -> do
    p <- newIORef (Node 0 p)
    putStrLn "node created"
    return p

testSTRef :: ST s String
testSTRef = do
    ref <- newSTRef "hello"
    x <- readSTRef ref
    writeSTRef ref " world"
    modifySTRef ref (x <>)
    readSTRef ref

-- class (Test2 a) => Test1 a where
-- class (Test1 a) => Test2 a where

-- type role Test3 representational
-- data Test3 a where
--     Test31 :: (Show a) => a -> Test3 a

f1 :: Char
Just f1 = Just 'a'

class A cls c where
  meth :: cls c => c -> c

class A B c => B c where

-- data List a = List a

-- f :: List (forall a. a -> a)
-- f = List undefined

last' = fix \f xs ->
    case xs of
      [] -> Nothing
      [x] -> Just x
      (x:xs) -> f xs

minus1 = fix  \f -> \case
        0 -> print 0
        n -> print n >> f (n - 1)

-- instance MonadFix Maybe where
--     mfix :: (a -> m b) -> m b
--     mfix f = let a = f (fromJust a) in a

data BTree = Z | B (IORef Int) BTree BTree
repsum t = do
    rec s <- rep_x_sum t s
    putStrLn ""
    return ()

rep_x_sum Z _ = return 0
rep_x_sum (B ref l r) s = do
  i <- readIORef ref
  writeIORef ref s
  putStr "("
  sl <- rep_x_sum l s
  putStr (show i)
  sr <- rep_x_sum r s
  putStr ")"
  return (i + sl + sr)

rectree = do
  r4 <- newIORef 4
  r3 <- newIORef 3
  r5 <- newIORef 5
  r1 <- newIORef 1
  let t = (B r4 (B r3 Z Z) (B r5 Z (B r1 Z Z)))
  repsum t
  repsum t
