{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Servant
import Servant.API.Generic

import Servant.Server
import Servant.Server.Generic
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import Network.Wai (Request)
import Data.Aeson (ToJSON, FromJSON)


main :: IO ()
main = putStrLn "Hello, Haskell!"


data User = User String String deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

type instance AuthServerData (AuthProtect "secret") = User


data Route route = Route
    { _login :: route
        :- "login"
        :> ReqBody '[JSON] User
        :> Post '[JSON] ()

    , _secret :: route
        :- AuthProtect "secret"
        :> Get '[JSON] String
    } deriving ( Generic )


authSecret :: AuthHandler Request User
authSecret = mkAuthHandler authHandler

authHandler :: Request -> Handler User
authHandler req = pure (User "name" "pwd")

