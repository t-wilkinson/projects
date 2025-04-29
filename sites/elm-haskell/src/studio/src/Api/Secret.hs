{-# LANGUAGE DeriveFunctor #-}

-- | Here lies secrets

module Api.Secret where

import Config (App, AppS)

import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant (AuthProtect, Headers, Handler, Header, addHeader, err404)
import Servant.API ( ResponseHeader(..))
import Web.Cookie
import Network.Wai (Request(..))
import Network.HTTP.Types ( hCookie )
import Data.List (head)
import Data.CaseInsensitive (original, mk)


data Login = Login
    { username :: String
    } deriving ( Eq, Show, Read, Generic )
instance ToJSON Login
instance FromJSON Login

data User = User String deriving (Eq,Show,Read,Generic)
instance ToJSON User
instance FromJSON User

type instance AuthServerData (AuthProtect "user") = User


data Route route = Route
  { _login :: route
    :- "login"
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

  , _admin :: route
    :- AuthProtect "user"
    :> "admin"
    :> Get '[JSON] String
  } deriving Generic

route = Route @AppS login admin


-- TODO we create a session cookie and send it back the the user
login :: App (Headers '[Header "Set-Cookie" SetCookie] ())
login = pure $ addHeader def { setCookieName = "id", setCookieValue = "random" } ()


admin :: User -> App String
admin (User name) = pure name

authHandler :: AuthHandler Request User
authHandler = mkAuthHandler authLookup

-- check for cookie with authentication
authLookup :: Request -> Handler User
authLookup pwd
  | cook == "bill" = pure (User "username")
  | otherwise = throw err404 {errBody = "hi"}
  where
      may = mapMaybe
        (\(k,v) -> if k == hCookie then Just v else Nothing)
        (requestHeaders pwd)
      cook = setCookieValue $ parseSetCookie (Data.List.head may)
