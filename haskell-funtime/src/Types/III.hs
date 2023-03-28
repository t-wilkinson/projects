{-# Language
  DataKinds
, GADTs
, KindSignatures
, TypeOperators
, RankNTypes
, PolyKinds
, TypeFamilies
, FlexibleInstances
, TypeApplications
, ScopedTypeVariables
, UndecidableInstances
, MultiParamTypeClasses
, FunctionalDependencies
, AllowAmbiguousTypes
, ConstraintKinds
#-}

module Types.III where

import Prelude hiding (fst)
import Data.Monoid ((<>))

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce


data HList (ts :: [Type]) where
    HNil :: HList ts
    (:#) :: t -> HList ts -> HList (t ': ts)
infixr 5 :#

test :: HList '[Int, Char]
test = (8::Int) :# 'i' :# HNil


data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type
    format :: String -> Proxy a -> Printf a

instance (KnownSymbol t) => HasPrintf (t :: Symbol) where
    type Printf t = String
    format s _ = s <> symbolVal (Proxy @t)

instance {-# Overlapping #-} (HasPrintf a) => HasPrintf (String :<< a) where
    type Printf (String :<< a) = String -> Printf a
    format s _ param = format (s <> param) (Proxy @a)

instance (KnownSymbol t, HasPrintf a) => HasPrintf ((t :: Symbol) :<< a) where
    type Printf (t :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @t)) (Proxy @a)

instance (HasPrintf a, Show t) => HasPrintf ((t :: Type) :<< a) where
    type Printf (t :<< a) = t -> Printf a
    format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""


{- 10. First Class Families -}
data Fst a b = Fst (a, b)

-- class Eval l t | l -> t where
--     eval :: l -> t

-- instance Eval (Fst a b) a where
--     eval (Fst (a, b)) = a

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

type instance Eval (Snd '(a, b)) = b

data List :: [a] -> Exp (Maybe a)
type instance Eval (List (a ': _)) = Just a
type instance Eval (List ('[])) = Nothing

data Pure :: a -> Exp a
type instance Eval (Pure a) = Exp a

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<


-- instance (TypeError
--     (Text "Attempting to interpret a number as a function "
--     :$$: Text "in the type `"
--     :<>: ShowType (a -> b)
--     :<>: Text "'"
--     :$$: Text "Did you forget to specify the function you wanted?"
--     )
--          ) => Num (a -> b)

