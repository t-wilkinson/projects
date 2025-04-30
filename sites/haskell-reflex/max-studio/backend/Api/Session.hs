module Api.Session where

import Quaalude

import Core.App (AppS, MonadLog(..), HashPassword(..))
import Core.Db
import Core.Config
import Db (runDb)

import qualified Database.Esqueleto as E
import Servant (err404, err409, err403)
import MyCookie
import Data.ByteString.Char8 (pack, unpack)
import Data.Serialize (Serialize)
import Crypto.KDF.BCrypt  (validatePassword)

data Valid = Valid | Invalid

data Login = Login
    { loginEmail :: String
    , loginPassword :: String
    } deriving (Generic, Show)
      deriving anyclass (ToJSON, FromJSON)

data Route r = Route
    { _login :: r
        :- "login"
        :> ReqBody '[JSON] Login
        :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] ())
    , _register :: r
        :- "register"
        :> ReqBody '[JSON] User
        :> Post '[JSON] (Headers '[Header "Set-Cookie" EncryptedSession] ())
    } deriving (Generic)

route = Route @AppS login register

login
    :: forall m.
        ( MonadIO m
        , MonadThrow m
        , MonadReader Config m
        )
    => Login
    -> m (Headers '[Header "Set-Cookie" EncryptedSession] ())
login user@Login{..} = do
    people <- runDb $
        E.select $ E.from $ \p -> do
            E.where_ (p E.^. UserEmail E.==. E.val loginEmail)
            pure p
    case validatePeople people loginPassword of
      Valid -> addSession' loginEmail ()
      Invalid -> throw err404 { errBody = "Passwords do no match" }

validatePeople :: [E.Entity User] -> String -> Valid
validatePeople people pwd =
    case people of
      [] -> throw err409 { errBody = "User does not exist" }
      (x:_) -> if validatePassword (pack pwd) (pack $ Core.Db.userPassword $ E.entityVal x)
        then Valid
        else Invalid

register
    :: ( MonadIO m
       , MonadReader Config m
       , MonadThrow m
       , HashPassword m
       )
    => User
    -> m (Headers '[Header "Set-Cookie" EncryptedSession] ())
register user@User{..} = do
    hashed <- hash userPassword
    (runDb $ E.insertUnique user { userPassword = hashed }) >>= \case
        Nothing -> throw err409 { errBody = "Username already exists" }
        Just _ -> addSession' userName ()

-- Helpers
addSession'
    :: ( MonadIO m
       , MonadThrow m
       , MonadReader Config m
       , Serialize a
       )
    => a -> r -> m (Headers '[Header "Set-Cookie" EncryptedSession] r)
addSession' a r = do
    cfg <- ask
    let rs = cfg^.randomSource
        sks = cfg^.serverKeySet
        acs = cfg^.authSettings
        ss = cfg^.sessionSettings
    addSession acs rs sks ss a r
