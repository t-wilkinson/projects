{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module CategoryTheory where

import Prelude hiding ( Monoid, Functor(..) )

class Monoid m where
    mu :: (m,m) -> m
    eta :: () -> m

newtype IntFun = IF (Int -> Int)

instance Monoid IntFun where
    mu (IF f, IF g) = IF (g . f)
    eta () = IF id

class Functor (f :: * -> *) where
    fmap :: (a -> a') -> f a -> f a'

class BiFunctor (f :: * -> * -> *) where
    dimap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

-- | Consumes on end, Outputs on the other
class Profunctor p where
    lmap :: (a' -> a) -> p a b -> p a' b
    lmap f = dmap f id
    rmap :: (b -> b') -> p a b -> p a b'
    rmap g = dmap id g
    dmap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
    dmap f g = rmap g . lmap f

instance Profunctor (->) where
    dmap con pro f = pro . f . con

data Test f a c = Test (a -> f c)

instance Functor f => Profunctor (Test f) where
    lmap f (Test g) = Test $ g . f
    rmap f (Test g) = Test (fmap f . g)

type End p = forall x. p x x

newtype NatPro f g a b = NatPro (f a -> g b)

instance (Functor f, Functor g) => Profunctor (NatPro f g) where
     dmap ba cd (NatPro p) = NatPro $ fmap cd . p . fmap ba

type Nat f g = End (NatPro f g)
-- type Nat f g = forall x. f x -> g x
data Coend p = forall x. Coend (p x x)
data Compose p q a b = forall x. Compose (p a x) (p x b)

-- instance (Profunctor p, Profunctor q) => Profunctor (Compose p q) where
--     dmap con pro (Compose pax qxb) = Compose (lmap con pax) (rmap pro 1xb)
