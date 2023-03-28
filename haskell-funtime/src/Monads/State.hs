{-# LANGUAGE TupleSections #-}
module Monads.State where

import           System.Random
import           Control.Applicative


newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s) = g s in (f a, s)

instance Applicative (State s) where
  pure a = State (a, )
  fs <*> xs = do
    f <- fs
    x <- xs
    return (f x)

instance Monad (State s) where
  a >>= f = State $ \s -> case runState a s of
    (a', s') -> runState (f a') s'


randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  --(,,) <$> randomSt <*> randomSt <*> randomSt
  liftA3 (,,) randomSt randomSt randomSt


stackManip' :: State Stack Int
stackManip' = do
  push' 3
  pop'
  pop'
  pop'
  pop'
  stackStuff
  pop'

stackStuff :: State Stack ()
stackStuff = do
  a <- pop'
  if a == 0 then push' 4 else push' 10


pop' :: State Stack Int
pop' = State $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = State $ \xs -> ((), a : xs)


type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
  let ((), stack1) = push 3 stack
      (a , stack2) = pop stack1
  in  pop stack2
