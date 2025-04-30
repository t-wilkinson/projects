module Api.User where

import Quaalude
import Core.Db
import Core.App (AppS, HashPassword(..), MonadLog(..))
import Common (Test(..))
import Core.Config
import Db (runDb)
import Servant (AuthProtect, err404, err409, err403)
import qualified Database.Esqueleto as E

import Crypto.Random.Types (MonadRandom(..))
import Crypto.KDF.BCrypt  (validatePassword)

import MyCookie
-- import MyCookie (AuthCookieSettings, AuthCookieException, ServerKey, getSession, ServerKeySet, ExtendedPayloadWrapper(..), encryptSession, mkRandomSource, mkPersistentServerKey, SessionSettings, addSession)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthHandler, AuthServerData)
import Network.Wai (Request)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Serialize (Serialize)

data Route r = Route
    { _admin :: r
        :- "admin"
        :> Post '[JSON] ()
    , _logout :: r
        :- "logout"
        :> Post '[JSON] ()
    , _rent :: r
        :- "rent"
        :> ReqBody '[JSON] (Maybe RentRequest)
        :> Post '[JSON] ()
    } deriving (Generic)

route = Route @AppS admin logout rent

admin
    :: ( MonadIO m
       , MonadLog m
       )
    => m ()
admin = do
    -- case value of
    --   (Just v) -> do
    --       log v
    --   _ -> pure ()
    pure ()

logout
    :: ( Monad m
       )
    => m ()
logout = undefined

rent
    :: (Monad m)
    => Maybe RentRequest
    -> m ()
rent rr = undefined

-- Helpers
cookieAuthCheck
    :: Config
    -> AuthHandler Request (Maybe String)
cookieAuthCheck cfg = mkAuthHandler $ \req -> do
    let sks = cfg^.serverKeySet
        acs = cfg^.authSettings
    (liftIO $ try $ getSession acs sks req) >>= \case
        Left ace -> do
            liftIO $ print (ace :: AuthCookieException)
            throw err403 { errBody = "Bad cookie" }
        Right maybeSession -> do
            liftIO $ print $ fmap epwSession maybeSession
            pure $ fmap epwSession maybeSession
