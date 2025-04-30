{-# OPTIONS_GHC -fno-warn-orphans #-}
module Quaalude
    ( module P
    , module X
    , type Year
    , type Month
    -- , type Day
    , identity
    , head
    -- , (?)
    ) where

import Prelude as P hiding (log, id, head)

import Servant.API as X
import Servant.API.Generic as X ((:-), AsApi, ToServantApi, ToServant, toServant, genericApi)
import Servant.Foreign (HasForeign(..))
import Servant.Server as X
import Servant.Server.Generic as X (AsServerT, genericServe)

import Control.Applicative as X
import Control.Exception as X (Exception)
import Control.Exception as X (bracket, try, throw)
import Control.Lens as X ((.~), (^.), (^?), view, set)
import Control.Monad as X
import Control.Monad.Catch as X (MonadThrow(..))
import Control.Monad.Except as X (ExceptT(..), MonadError, throwError, catchError)
import Control.Monad.Fix as X (MonadFix(..), mfix)
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X ( Reader, ReaderT(..), MonadReader, ask, asks, runReaderT )
import Control.Monad.State as X ( State, StateT )
import Control.Monad.Trans.Cont as X (ContT(..), runContT)
import Control.Monad.Trans.Maybe as X (MaybeT(..))

import Control.Monad.ST as X
import Data.Aeson as X (FromJSON, ToJSON, fromJSON, toJSON, encode, decode)
import Data.Bifunctor as X (bimap)
import Data.CaseInsensitive as X (CI)
import Data.Char as X (toLower, toUpper)
import Data.Default as X (Default, def)
import Data.Foldable as X (traverse_, toList)
import Data.Function as X
import Data.Functor as X
import Data.Functor.Identity as X
import Data.IORef as X
import Data.Int as X
import Data.Kind as X
import Data.List as X hiding (head)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Maybe as X
import Data.Proxy as X (Proxy(..))
import Data.String as X
import Data.Time as X (DiffTime, UTCTime(..), TimeOfDay, getCurrentTime, fromGregorian, secondsToDiffTime)
import Data.Traversable as X
import Data.Void as X (Void)

import Network.HTTP.Simple as X (httpBS)
import Network.HTTP.Client as X (httpLbs)
import Debug.Trace as X (trace, traceId, traceShow, traceShowId, traceM, traceShowM)
import GHC.Conc as X (pseq, par)
import GHC.Generics as X (Generic(..))
import GHC.TypeLits as X  -- Data Kinds
import System.Environment as X
import Text.Read as X (readMaybe)

instance (KnownSymbol sym, HasForeign lang ftype sub)
  ⇒ HasForeign lang ftype (AuthProtect sym :> sub) where
    type Foreign ftype (AuthProtect sym :> sub) = Foreign ftype sub
    foreignFor lang ftype _ req = foreignFor lang ftype (Proxy @sub) req

type Year = String
type Month = String
-- type Day = String

identity ∷ a → a
identity a = a

head ∷ [a] → Maybe a
head [] = Nothing
head (x:_) = Just x

