module Quaalude
    ( module P
    , module X
    , identity
    ) where

import Prelude as P hiding (log, id)
import Servant.API as X ( (:>)
                          , ReqBody, Capture
                          , JSON, PlainText, FormUrlEncoded, OctetStream
                          , Put, Delete, Get, Post
                          , Headers, Header, addHeader
                          )
import Servant.API.Generic as X ( (:-) )
import Servant.Server as X ( errHTTPCode, errReasonPhrase, errBody, errHeaders )
import Servant.Server.Generic as X (AsServerT, genericServe)

import Control.Monad.Catch as X (MonadThrow(..))
import Control.Applicative as X
import Control.Exception as X (try, throw)
import Control.Lens as X ((&), (.~), (^.), view, set)
import Control.Monad.Except as X
import Control.Monad.IO.Class as X
import Control.Monad.Reader as X ( Reader, ReaderT(..), MonadReader, ask, asks, runReaderT )
import Control.Monad.State as X ( State, StateT )
import Data.Aeson as X (FromJSON, ToJSON)
import Data.Default as X (Default, def)
import Data.Functor as X
import Data.Int as X
import Data.Maybe as X
import Data.Proxy as X (Proxy(..))
import Data.String as X
import Data.Text as X (Text)
import Data.Text.IO as TextIO
import Data.Time as X (UTCTime, TimeOfDay, Day)
import GHC.Generics as X (Generic(..))
import System.IO as X ()

identity :: a -> a
identity a = a

-- class Print a where
--     putStr :: MonadIO m => a -> m ()
--     putStrLn :: MonadIO m => a -> m ()

-- instance Print X.Text where
--   putStr = liftIO . TextIO.putStr
--   putStrLn = liftIO . TextIO.putStrLn

-- print :: (X.MonadIO m, Show a) => a -> m ()
-- print = liftIO . P.print


