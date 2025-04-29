module Monads.Writer where

newtype Writer w a = Writer { runWriter :: (a,w)}

instance Functor (Writer w) where
  fmap f w = case runWriter w of
    (x, v) -> Writer (f x, v)

instance (Monoid w) => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  f <*> v =
    let (f', v' ) = runWriter f
        (a , v'') = runWriter v
    in  Writer (f' a, v' `mappend` v'')

instance (Monoid w) => Monad (Writer w) where
  return = pure
  (Writer (x, v)) >>= f =
    let (Writer (y, v')) = f x in Writer (y, v `mappend` v)
