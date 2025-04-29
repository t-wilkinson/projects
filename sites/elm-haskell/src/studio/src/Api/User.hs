{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Authorize requests using token

module Api.User where

import Config ( App )
import Db.User
import Db ( runDb )

import Control.Monad.Except ( MonadError )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
-- import Crypto.BCrypt ( HashingPolicy(..), hashPassword
--                      , validatePassword, genSaltUsingPolicy
--                      , hashPasswordUsingPolicy
                     -- )
import Data.Aeson ( Value (..) )
import Data.ByteString.Char8 ( pack, unpack)
-- import Codec.Binary.UTF8.String ( encode, decode )
-- import Data.ByteArray ( ByteArray(..), pack, unpack )
import Servant ( err401, err406, err404, err500 )
import Crypto.Random.Types ( MonadRandom(..) )
import Crypto.KDF.BCrypt  ( hashPassword, validatePassword, bcrypt )
import Database.Persist

type Username = String
type Password = String
type Hash = String
type Token = String


data Route route = Route
  { -- | Login, returns token upon succesful request
    _login :: route
    :- "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] Token

    -- | Register user, return token on successful request
  , _register :: route
    :- "register"
    :> ReqBody '[JSON] Register
    :> Post '[JSON] Token

    -- | Logout
  , _logout :: route
    :- "logout"
    :> Capture "token" Token
    :> Get '[JSON] ()
  } deriving Generic

route = Route @(AsServerT App) login register logout


toToken :: MonadRandom m => Password -> m Token
toToken password = do
  unpack <$> hashPassword 10 (pack password)


login :: Login -> App Token
login Login{loginUsername, loginPassword} = do
  token <- liftIO $ toToken loginPassword
  runDb $ selectFirst [ UserUsername ==. loginUsername
                      ] [] >>= \case
    Nothing -> throw err404 {errBody = "User not found" }
    Just (Entity _ value) ->
      if validatePassword (pack loginPassword)  (pack $ userPassword value)
        then pure token
        else throw err404 { errBody = "Passwords do no match" }


register :: Register -> App Token
register Register{registerUsername, registerPassword} = do
  token <- liftIO $ toToken registerPassword
  runDb $ insertUnique (User registerUsername token) >>= \case
    Nothing -> throw err406 { errBody = "Username in use" }
    Just _ -> pure token


logout :: Token -> App ()
logout token = do
  runDb $ selectList [ UserPassword ==. token ] [] >>= \case
    [] -> throw err404 { errBody = "User not found" }
    _ -> pure ()

