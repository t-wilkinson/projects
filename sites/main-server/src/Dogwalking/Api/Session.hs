{-# LANGUAGE TemplateHaskell #-}
{-# Language DuplicateRecordFields #-}

module Dogwalking.Api.Session where

-- Local imports
import Quaalude
import Core.Session (toSession, toCookieValue)
import Core.Config (Config, sk)
import Core.Helpers (addUserCookie, getMatchingRecordsByField, persistCookie, sessionCookie)
import Core.App (AppS, HashPassword(..))
import Core.Session (userField, parseSessions, keyToBS, sesField, sesToken)
import Dogwalking.Db
import Core.Token (SToken, Token, tokenize, untokenize, toSToken)
import qualified Core.Errors as CE

-- Outer
import Crypto.KDF.BCrypt (validatePassword)
import Data.Validation (Validation(..), validation)
import Web.Cookie
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Data.Aeson  as JSON
import qualified Data.Aeson.TH as JSON

data UserLogin = UserLogin
    { email ∷ String
    , password ∷ String
    , remember ∷ Bool
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data UserRegister = UserRegister
    { email ∷ String
    , password ∷ String
    , remember ∷ Bool
    , streetAddress ∷ String
    , city ∷ String
    , state ∷ String
    , zipCode ∷ Int
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data ReturnUser = ReturnUser
    { email ∷ String
    , token ∷ SToken
    , session ∷ String
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data UserValidate = UserValidate
    { email ∷ Maybe String
    , password ∷ Maybe String
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data UserLoginErrors = UserLoginErrors
    { emailErrors ∷ [String]
    , passwordErrors ∷ [String]
    } deriving (Generic)

JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = \xs → take (length xs - 6) xs }) ''UserLoginErrors

data GetUser = GetUser
    { token ∷ SToken
    , email ∷ String
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data Route r = Route
    { -- Session stuff
      _login ∷ r
        :- "login"
        :> Description "Login route"
        :> Header' '[Strict, Optional] "cookie" BS8.ByteString
        :> ReqBody '[JSON] UserLogin
        :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie
                                  ] ReturnUser)
    , _register ∷ r
        :- "register"
        :> Description "Register route"
        :> Header' '[Strict, Optional] "cookie" BS8.ByteString
        :> ReqBody '[JSON] UserRegister
        :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                  , Header "Set-Cookie" SetCookie
                                  ] ReturnUser)
    , _logout ∷ r
        :- "logout"
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

    -- Acquire information about user
    , _getUser ∷ r
        :- "user"
        :> Header' '[Strict, Optional] "cookie" BS8.ByteString
        :> Get '[JSON] (Maybe GetUser)
    , _getUsers ∷ r
        :- "users"
        :> Header' '[Strict, Optional] "cookie" BS8.ByteString
        :> Get '[JSON] [(String, String)]
    , _changeUser ∷ r
        :- "user"
        :> QueryParam' '[Strict, Required] "userToken" Token
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ())

    , _getLoginFieldErrors ∷ r
        :- "validate"
        :> ReqBody '[JSON] UserValidate
        :> Post '[JSON] UserLoginErrors
    } deriving (Generic)

route ∷ Route AppS
route = Route
    login
    register
    logout
    getUser
    getUsers
    changeUser
    getLoginFieldErrors

login
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , HashPassword m
        , MonadError ServerError m
        )
    ⇒ Maybe BS8.ByteString
    → UserLogin
    → m (Headers '[ Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] ReturnUser)
login maybeCookies UserLogin{..} = do
    sk' ← asks (^.sk)
    users ← runDb $ getMatchingRecordsByField UserEmail email
    let maybeValid = case users of
            [] → Nothing
            (x:_) → if validatePassword (BS8.pack password) (BS8.pack $ userPassword $ E.entityVal x)
                       then Just x
                       else Nothing
    entityUser ← maybe
        (throwError CE.invalidCredentials)
        pure
        maybeValid

    let session = toSession sk' (E.entityKey entityUser) ""
        returnUser = ReturnUser email (toSToken $ sesToken session) (BS8.unpack $ toCookieValue session)
    pure $ if remember
              then addUserCookie session maybeCookies returnUser
              else noHeader $ noHeader $ returnUser

register
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , HashPassword m
        , MonadError ServerError m
        )
    ⇒ Maybe BS8.ByteString
    → UserRegister
    → m (Headers '[ Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] ReturnUser)
register maybeCookies UserRegister{..} = do
    sk' ← asks (^. sk)

    -- validate info
    case (vEmail email,vPassword password) of
      (Success _, Success _) → pure ()
      _ → throwError CE.parsingCredentials

    -- insert user
    hashed ← hash password
    key ← (runDb $ E.insertUnique $ User email hashed streetAddress city state zipCode) >>= maybe (throwError CE.accountExists) pure

    -- return header
    let session = toSession sk' key ""
        returnUser = ReturnUser email (toSToken $ sesToken session) (BS8.unpack $ toCookieValue session)
    pure $ if remember
              then addUserCookie session maybeCookies returnUser
              else noHeader $ noHeader $ returnUser

logout
    ∷ ∀ m.
        ( Monad m
        )
    ⇒ m (Headers '[Header "Set-Cookie" SetCookie] ())
logout = pure $ addHeader (sessionCookie userField "") ()

changeUser
    ∷ ∀ m.
        ( Monad m
        )
    ⇒ Token
    → m (Headers '[Header "Set-Cookie" SetCookie] ())
changeUser userToken = pure $ addHeader (persistCookie userField userToken) ()

getUser
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
   ⇒ Maybe BS8.ByteString
   → m (Maybe GetUser)
getUser maybeSessions = do
    sk' ← asks (^.sk)
    let maybeKeyToken = do
           sessions ← maybeSessions
           token ← lookup userField $ parseCookies $ sessions
           key ← JSON.decode $ BSL8.fromStrict $ untokenize sk' token
           pure (key, token)
    case maybeKeyToken of
        Nothing → pure Nothing
        Just (key,token) → do
            maybeUser ← runDb $ P.get key
            pure do
               user ← maybeUser
               Just $ GetUser (BS8.unpack token) (userEmail user)

getUsers
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Maybe BS8.ByteString
    → m [(String, String)]
getUsers Nothing = pure []
getUsers (Just sessions) = do
    sk' ← asks(^.sk)
    case (lookup sesField . parseCookies $ sessions) of
      Nothing → pure []
      Just field → do
        let keys ∷ [Key User]
            keys = catMaybes $ (keyFromToken sk') <$> parseSessions field
        keyUsers ← Map.toList <$> runDb (P.getMany keys)
        pure $ fmap
                (\(key, user) → (keyToToken sk' key, userEmail user))
                keyUsers
  where
    keyFromToken sk' = head >=> JSON.decode . BSL8.fromStrict . untokenize sk'
    keyToToken sk' = BS8.unpack . tokenize sk' . keyToBS

getLoginFieldErrors
    ∷ (Monad m)
    ⇒ UserValidate
    → m UserLoginErrors
getLoginFieldErrors UserValidate{email, password} = do
    let emailE = toError vEmail email
        passE = toError vPassword password
    pure $ UserLoginErrors emailE passE

toError ∷ ∀ a. (a → Validation [Error] a) → Maybe a → [String]
toError f = maybe [] $ validation (fmap show) (const []) . f

data Error
    = TooShort Int
    | TooLong Int
    | MustContain String
    | MustNotContain String
    | MustNotBeEmpty

instance Show Error where
    show (TooShort i) = "Must be at least " <> show i <> " characters long"
    show (TooLong i) = "Can not be more than " <> show i <> " characters long"
    show (MustContain s) = "Must include \"" <> s <> "\""
    show (MustNotContain s) = "Must not contain a \"" <> s <> "\""
    show (MustNotBeEmpty) = "Must not be empty"

type Validate = String → Validation [Error] String

vTooShort ∷ Int → Validate
vTooShort n x = if length x >= n
                   then Success x
                   else Failure [TooShort n]

vTooLong ∷ Int → Validate
vTooLong n x = if length x <= n
                   then Success x
                   else Failure [TooLong n]

vHasChar ∷ String → Validate
vHasChar c x = if all (`elem` x) c
                  then Success x
                  else Failure [MustContain c]

vNoHasChar ∷ String → Validate
vNoHasChar c x = if any (`elem` x) c
                  then Failure [MustNotContain c]
                  else Success x

vNonEmpty ∷ Validate
vNonEmpty [] = Failure [MustNotBeEmpty]
vNonEmpty xs = Success xs

vClean ∷ String → String
vClean = T.unpack . T.strip . T.pack

vEmail ∷ Validate
vEmail x = pure x
    <* vNonEmpty x
    <* vHasChar "@." x
    <* vNoHasChar " =;<>[]{}\"\\/"
    (vClean x)

vPassword ∷ Validate
vPassword x = pure x
    <* vTooShort 4 x
    <* vTooLong 18 x
    <* vNoHasChar " =;<>[]{}\"\\/"
    (vClean x)
