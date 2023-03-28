module Monads.Reader where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Map                      as Map


addStuff :: Int -> Int
addStuff = do
  a <- (+ 3)
  b <- (* 10)
  c <- (+ 2)
  -- d <- id
  return (a + b)


-- class Monad m => MonadReader r m | m -> r where
-- r is *determined* from m

type Bindings = Map String Int

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Mine Bool
isCountCorrect bindings = runReaderT calcIsCountCorrect bindings

newtype Mine a = Mine { getItem :: a }

instance Functor Mine where
  fmap f (Mine a) = Mine $ f a

instance Applicative Mine where
  pure = Mine
  Mine f <*> Mine a = Mine $ f a

instance Monad Mine where
  Mine a >>= f = f a

-- The Reader monad, which implements this complicated check.
calcIsCountCorrect :: ReaderT Bindings Mine Bool
calcIsCountCorrect = do
  count    <- asks (lookupVar "count")
  bindings <- ask
  return (count == Map.size bindings)

-- The selector function to  use with 'asks'.
-- Returns value of the variable with specified name.
lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (Map.lookup name bindings)

sampleBindings = Map.fromList [("count", 3), ("1", 1), ("b", 2)]

test :: IO ()
test = do
  putStr $ "Count is correct for bindings " ++ show sampleBindings ++ ": "
  print (getItem $ isCountCorrect sampleBindings)
