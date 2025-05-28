{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | Thinking with types

module Types where

import           Data.Coerce                    ( Coercible(..)
                                                , coerce
                                                )
import           Data.Foldable                  ( toList )
import qualified Data.Map                      as M
import           Data.Monoid                    ( Sum(..)
                                                , Product(..)
                                                )
import           Data.IORef
import           System.IO.Unsafe               ( unsafePerformIO )
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromMaybe )
import           Data.Typeable           hiding ( Proxy )
import           Data.Kind                      ( Constraint
                                                , Type
                                                )

data Proxy a = Proxy

{- Chapter 1 The Algebra Behind Types -}
{- Chapter 2 Terms, Types, and Kinds -}
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

{- Chapter 3 Variance -}
{- Chapter 4 Working with Types -}
working :: forall a b . (a -> b) -> a -> b
working f a = apply
 where
  apply :: b
  apply = f a


types :: IO ()
types = do
  print $ typeRep (Proxy :: Proxy Int)
  print $ typeName @Bool


typeName :: forall a . Typeable a => String
typeName = show . typeRep $ Proxy @a


{- Chapter 5 Constraints and GADTs -}
-- (~) :: reflexive symmetric transitive

five :: Int
five = 5

five' :: (a ~ Int) => a
five' = 5

data Expr a where
  LitInt ::Int -> Expr Int
  LitBool ::Bool -> Expr Bool
  Add ::Expr Int -> Expr Int -> Expr Int
  Not ::Expr Bool -> Expr Bool
  If ::Expr Bool -> Expr a -> Expr a -> Expr a

data Expr' a
  = (a ~ Int) => LitInt' Int
  | (a ~ Bool) => LitBool' Bool
  | (a ~ Int) => Add' (Expr' Int) (Expr' Int)
  | (a ~ Bool) => Not' (Expr' Bool)
  | If' (Expr' Bool) (Expr' a) (Expr' a)

evalExpr :: Expr a -> a
evalExpr (LitInt  n) = n
evalExpr (LitBool b) = b
evalExpr (Add x y  ) = evalExpr x + evalExpr y
evalExpr (Not b    ) = not $ evalExpr b
evalExpr (If b x y ) = if evalExpr b then evalExpr x else evalExpr y


data HList (ts :: [Type]) where
  HNil ::HList '[]
  (:#) ::t -> HList ts -> HList (t ': ts)
infixr 5 :#

{-
type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)
-}

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil      == HNil      = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance All Show ts => Show (HList ts) where
  show HNil      = "HNil"
  show (a :# as) = show a <> " :# " <> show as


hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

intFirst :: (Int -> a) -> HList (Int ': ts) -> a
intFirst f (t :# _) = f t

{- Chapter 6 Rank-N Types -}
-- id :: forall a . a -> a
-- id a = a
-- applyToFive :: forall a . (a -> a) -> Int
-- can be ANY a

applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5

newtype Cont a = Cont
    { unCont :: forall r . (a -> r) -> r }

newtype ContT m a = ContT
    { runCont :: forall r . (a -> m r) -> m r }

instance Functor Cont where
  fmap f (Cont c) = Cont $ \c' -> c (c' . f)


{- Chapter 7 Existential Types -}
{- 7.1 -}
data Any = forall a. Any a
data Any' where
  Any' :: a -> Any'
-- eliminator :: rank-2 function which takes an existential type and a
-- continuation that can produce a value regardless of what it gets.
elimAny :: (forall a . a -> r) -> Any -> r
elimAny f (Any a) = f a

-- data HasShow where
--   HasShow :: Show t => t -> HasShow

-- instance Show HasShow where
--   show (HasShow s) = "HasShow " <> show s

-- elimHasShow :: (forall a . Show a => a -> r) -> HasShow -> r
-- elimHasShow f (HasShow a) = f a

data Dynamic where
  Dynamic ::Typeable t => t -> Dynamic


elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

-- cast :: (Typeable a, Typeable b) => a -> Maybe b
fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
  :: forall a b r
   . (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "bad types for pyPlus") $ asum
  [ liftD2 @String @String a b (++)
  , liftD2 @Int @Int a b (+)
  , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
  , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
  ]


data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a . c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

-- type HasShow = Has Show
-- type Dynamic = Has Typeable

class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a


{- 7.2 -}
newtype ST s a = ST
  { unsafeRunST :: a }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s ) where
  ST a >>= f = seq a $ f a

newtype STRef s a = STRef
  { unSTRef :: IORef a }


data MyInt where
  MyIn ::Show a => a -> MyInt

instance Show  MyInt where
  show (MyIn a) = show a

add
  :: forall r . Semigroup r
  => (forall a . Show a => a -> r)
  -> MyInt
  -> MyInt
  -> r
add f (MyIn a) (MyIn b) = f a <> f b



{- 8 Roles -}
{- 8.1 Coercions -}
-- coerce :: Coercible a b => a -> b


-- fmap NewtypeConstructor <=> coerce

data Mine a = Mine

type role Mine phantom

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

data BST v
  = Empty
  | Branch (BST v) v (BST v)

type role BST nominal


-- sm uniquely determines m WIHTOUT having to be a type constructor
class Ape sm m | sm -> m where
  apeHead :: sm m -> m
  apeTail :: sm m -> sm m


data family Ape' (sm :: * -> *) m
  apeHead' :: sm m -> m

withOS :: (String -> r) -> r
withOS f = f "linux"

withOS' :: (String -> String) -> String
withOS' f = f "linux"

