{-# Language
  GADTs
#-}

module Monads.Free where

data Toy b next
    = Output b next
    | Bell next
    | Done
    deriving (Show)

-- data Fix f = Fix (f (Fix f))
data FixE f e
    = Fix (f (FixE f e))
    | Throw e

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell next) = Bell (f next)
    fmap f Done = Done

fix :: (a -> a) -> a
fix f = let x = f x in x

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (flip catch f) x)
catch (Throw e) f = f e

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output x = liftF (Output x())
bell = liftF (Bell ())
done = liftF Done

program :: Free (Toy Char) r
program = do
    output 'A'
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"

-- why is there no type class for destructuring functor like objects

-- class Functor f => Destructure f where
--     destructure (f a) = a

-- GADTs
data Free f r where
    Free :: Functor f => f (Free f r) -> Free f r
    Pure :: Functor f =>  r -> Free f r

data FreeT r = PureT r | FreeT r (FreeT r)

liftFree :: Functor f => f a -> Free f a
liftFree f = Free $ fmap Pure f

foldFree :: Functor f => (f r -> r) -> Free f r -> r
foldFree _ (Pure a) = a
foldFree f (Free x) = f $ fmap (foldFree f) x

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure a <*> Pure b = Pure $ a b
    Pure a <*> Free mb = Free $ fmap a <$> mb
    Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)
