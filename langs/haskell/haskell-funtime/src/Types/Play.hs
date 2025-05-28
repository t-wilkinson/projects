{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Types.Play where

import           Data.Kind                      ( Type, Constraint )
import           Data.Foldable                  ( traverse_ )
import           Data.Typeable                  ( (:~:)(Refl), eqT, cast, Typeable, Proxy(..))


main :: IO ()
main = do
  traverse_
    (print . show)
    [typeName @Int, show $ rank3 (\f -> f @Integer 80), show $ rank2 id]


data Phantom a = Phantom deriving (Show)
data NotShow = NotShow

type Id = forall a . a -> a
id' = id :: Id

-- Just :: a -> Maybe a
-- Just 8 :: Maybe Int
-- Maybe :: * -> *

-- 'J :: a -> May a
-- 'J Int :: May *
-- May :: * -> *

-- data MyType = MyType
-- data May a = J a | N
-- data HList (ts :: [Type]) where
--   HNil :: HList '[]
--   (:#) :: t -> HList ts -> HList (t ': ts)
-- infix 5 :#

typeName :: forall a . Show a => String
typeName = show (Phantom @a)


-- The calling function of the calling function determines the type of f
rank3 :: ((forall a . a -> a) -> Integer) -> Integer
rank3 f = f id

rank2 :: (forall a . a -> a) -> Integer
rank2 f = f 3
-- rank3 f = f (id @Integer) <-- Doesn't work


data Nat = Succ Nat | Zero
data Vec :: Nat -> * where
  Nil ::Vec 'Zero
  Cons ::Int -> Vec n -> Vec ('Succ n)

type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True


data FamMember
  = Child
  | Parent
class Family a where
  data LoseChild a
  type GetChild a :: * -> *
  logger :: String -> (GetChild a) ()
  loseChild :: FamMember -> LoseChild a -> FamMember


-- data HList' (ts :: [Type]) where
--   HNil' ::HList' '[]
--   (:::) ::t -> HList' ts -> HList' (t ': ts)

-- infix 5 :::

cast :: forall a b . (Typeable a, Typeable b) => a -> Maybe b
cast x = case eqT :: Maybe (a :~: b) of
  Nothing   -> Nothing
  Just Refl -> Just x

f :: forall a . Typeable a => a -> a
f x = case eqT :: Maybe (a :~: Int) of
  Nothing   -> x
  Just Refl -> x + 1

g :: forall a . Typeable a => a -> a
g x = case Data.Typeable.cast x :: Maybe Int of
  Nothing -> x
  Just n  -> case Data.Typeable.cast (n + 1) :: Maybe a of
    Nothing -> x
    Just y  -> y


fix :: (a -> a) -> a
fix f = let x = f x in x

-- fix' :: Monad m => (a -> m a) -> m a
-- fix' f = let { x = f x } in x

data Exis where
  Exis :: forall m a. (Show a, Monad m) => m a -> Exis

exis :: [Exis]
exis = [Exis (Just (8 :: Int)), Exis ['a' .. '5']]



{- printf -}
data Has (c :: Type -> Constraint) where
    Has :: c a => a -> Has c

newtype NoQuotes = NoQuotes String

instance Show NoQuotes where
    show (NoQuotes s) = s

printf :: String -> [Has Show] -> String
printf ('%':ss) (Has x:xs) = show x <> printf ss xs
printf (s:ss) xs = s:printf ss xs
printf string _ = string

test :: String
test = printf "hello %, you are % years old" [Has (NoQuotes "bob"), Has 8, Has 'c']


{- variable arguments -}
foo :: FooType a => a
foo = bar (pure ())

class FooType a where
    bar :: IO () -> a

instance FooType (IO ()) where
    bar = id

instance (Show x, FooType r) => FooType (x -> r) where
    bar s x = bar (s >> print x)

data Test = Will | Bob
-- kind Test = 'Will | 'Bob

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True x = 'True
    Or 'False x = x

data HList (t :: [Type]) where
    (:#) :: t -> HList ts -> HList (t ': ts)
    HNil :: HList '[]
infix 5 :#
