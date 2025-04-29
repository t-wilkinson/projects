{-# LANGUAGE
  DataKinds
, DeriveAnyClass
, DerivingStrategies
#-}

module Api.Secret where

import Quaalude
import Core.App ( AppS )

import Network.HTTP.Types ( hCookie )
import Network.Wai (Request(..))
import Servant (AuthProtect, Headers, Handler, Header, addHeader, err404)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie

import Servant ( JSON )

data Login = Login
    { username :: String
    } deriving ( Eq, Show, Read, Generic )
      deriving anyclass ( ToJSON, FromJSON )

data User = User String
    deriving (Eq,Show,Read,Generic)
    deriving anyclass ( ToJSON, FromJSON )

type instance AuthServerData (AuthProtect "hidden") = User

data Route r = Route
  { _login :: r
    :- "loginyo"
    :> Post '[JSON] (Headers '[Header "Set-Cookie" String] ())
  , _admin :: r
    :- "admin"
    :> AuthProtect "hidden"
    :> Get '[JSON] User
  } deriving Generic

route = Route @AppS login admin


-- TODO we create a session cookie and send it back the the user
login :: (Monad m) => m (Headers '[Header "Set-Cookie" String] ())
login = pure $ addHeader "bill=5" ()
-- login = pure $ addHeader def { setCookieName = "id", setCookieValue="random" } ()

admin :: (Monad m) => User -> m User
admin (User will) = pure (User will)

authHandler :: AuthHandler Request User
authHandler = mkAuthHandler authLookup

-- check for cookie with authentication
authLookup :: Request -> Handler User
authLookup req
  | value == "bill" = pure (User "username")
  | otherwise = throw err404 {errBody = "hi"}
  where
      maybeCookie = mapMaybe
        (\(k,v) -> if k == hCookie then Just v else Nothing)
        (requestHeaders req)
      value = setCookieValue $ parseSetCookie (head maybeCookie)

