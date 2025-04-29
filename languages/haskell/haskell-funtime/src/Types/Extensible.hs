{-# Language
  AllowAmbiguousTypes
, ConstraintKinds
, DataKinds
, FlexibleContexts
, FlexibleInstances
, GADTs
, KindSignatures
, MultiParamTypeClasses
, OverloadedLabels
, PolyKinds
, RankNTypes
, ScopedTypeVariables
, TypeApplications
, TypeFamilies
, TypeOperators
, UndecidableInstances
#-}

module Types.Extensible where

import Data.Kind (Type, Constraint)
import Data.Proxy
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce
import Fcf hiding (Any)


{- 11. Extensible Data Types -}
{- Sum -}
data OpenSum (f :: k -> Type) (ts :: [k]) where
    UnsafeOpenSum :: Int -> f t -> OpenSum f ts
type FindElemSum (key :: k) (ts :: [k]) =
    FromMaybe Stuck =<< FindIndex (TyEq key) ts
type Member t ts = KnownNat (Eval (FindElemSum t ts))

findElemSum :: forall t ts. Member t ts => Int
findElemSum = fromIntegral . natVal $ Proxy @(Eval (FindElemSum t ts))

inj :: forall f t ts. Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElemSum @t @ts)

prj :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) = if i == findElemSum @t @ts
                             then Just $ unsafeCoerce f
                             else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

{- Product -}
data Any (f :: k -> Type) where
    Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
    OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert
    :: Eval (UniqueKey key ts) ~ 'True
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
    OpenProduct $ V.cons (Any ft) v

type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem
    :: forall key ts. KnownNat (FindElem key ts)
    => Int
findElem =
    fromIntegral
    . natVal
    $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
    FromMaybe Stuck =<< Lookup key ts

get
    :: forall key ts f. KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
    unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
    SetIndex (FindElem key ts) '(key, t) ts

update
    :: forall key ts t f. KnownNat (FindElem key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
    OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Filter (Not <=< TyEq key <=< Fst) ts

delete
    :: forall key ts t f. KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) = OpenProduct v
-- does not remove elem in vector

instance (key ~ key') => IsLabel key (Key key') where
    fromLabel = Key

