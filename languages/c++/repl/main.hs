{-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE GADTs #-}
        {-# LANGUAGE ScopedTypeVariables #-}
            {-# LANGUAGE TypeFamilies #-}

import Data.Typeable

newtype State s m a = State { runState :: (s -> m (a, s)) }

instance Monad m => Functor (State s m) where
  fmap f (State g) = State $ \s1 ->
      let ma = g s1
      in fmap (\ ~(a,s2) -> (f a, s2)) ma

instance Monad m => Applicative (State s m) where
    pure a = State $ \s -> pure (a, s)
    (State mf) <*> (State ma) = State $ \s -> do
      ~(f,s') <- mf s
      ~(a,s'') <- ma s'
      pure (f a, s'')

instance Monad m => Monad (State s m) where
  (State mb) >>= f = State $ \s -> do
      ~(a,s') <- mb s
      runState (f a) s'

main :: IO ()
main = pure ()

test ::  a ->  ()
test a = case undefined :: (a :~: Int) of
             (Refl) -> ()

class Category c where
  data Object c
  data Mor c
  ob :: [Object c]
  mor :: [Mor c]
  identity :: Mor c
  comp :: Mor c -> Mor c -> Mor c

