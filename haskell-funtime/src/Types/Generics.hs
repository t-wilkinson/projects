{-# Language
  AllowAmbiguousTypes
, DataKinds
, DefaultSignatures
, DeriveAnyClass
, DeriveGeneric
, DeriveGeneric
, FlexibleContexts
, FlexibleContexts
, FlexibleInstances
, KindSignatures
, OverloadedStrings
, ScopedTypeVariables
, StandaloneDeriving
, TypeApplications
, TypeFamilies
, TypeOperators
, TypeOperators
, UndecidableInstances
#-}

module Types.Generics where

import GHC.Generics
import Control.Monad.Writer
import Data.Aeson (Value(..), (.=), object)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err

class GEq a where
    geq :: a x -> a x -> Bool

instance GEq U1 where
    geq U1 U1 = True

instance Eq a => GEq (K1 _1 a) where
    geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L1 a1) (L1 a2) = geq a1 a2
    geq (R1 a1) (R1 a2) = geq a1 a2
    geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance (GEq a) => GEq (M1 _x _y a) where
    geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

data Loo a b c = F1 | F2 a | F3 a b c
    deriving (Generic)

instance (Eq a, Eq b, Eq c) => Eq (Loo a b c) where
    (==) = genericEq

class GOrd a where
    gord :: a x -> a x -> Bool

class MyEq a where
    myeq :: a -> a -> Bool
    default myeq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
    myeq a b = geq (from a) (from b)


class GSchema (a :: Type -> Type) where
    gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired
    :: forall nm. KnownSymbol nm
    => Writer [Text] ()
emitRequired = tell. pure. pack. symbolVal $ Proxy @nm


