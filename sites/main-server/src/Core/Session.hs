{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Core.Session where

import Quaalude

import Core.Config (ServerKey, PrivateID)
import Core.Token (Token, tokenize, untokenize)

import Crypto.Hash.Algorithms (SHA3_224(..))
import Crypto.MAC.HMAC (hmac, HMAC(..))
import Network.HTTP.Types (hCookie)
import Web.Cookie
import Database.Persist (PersistEntity, Key)
import Data.ByteString.Char8 (ByteString)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL8
import qualified Network.HTTP.Types as HTTP

-- cookies :=
-- key=<key>
-- sessions=<key>|data|HMAC(key|data|ip),<key>|data|HMAC(key|data|ip)

data Session record = Session  -- <key>|data|HMAC(key|data|ip)
    { sesKey ∷ Key record
    , sesToken ∷ Token
    , sesPrivateID ∷ PrivateID
    , sesData ∷ ByteString
    , sesHMAC ∷ HMAC SHA3_224
    }

instance Show (Session record) where
    show Session{..} = "token:" <> BS8.unpack sesToken
        <> "\nprivateId:" <> BS8.unpack sesPrivateID
        <> "\nsesData:" <> BS8.unpack sesData
        <> "\nsesHMAC:" <> (show $ hmacGetDigest $ sesHMAC)

-- | Various fields to reduce mix-ups
userField = "key"
sesField = "sessions"
sesOuterSep = ','
sesInnerSep = '|'

-- | Create a 'Session' from source values
-- requires computing a 'HMAC'
toSession
    ∷ ∀ record.  ( PersistEntity record)
    ⇒ ServerKey → Key record
    → ByteString → Session record
toSession sk key sesData =
    Session key tokenKey privateID sesData (hash sk tokenKey privateID sesData)
  where
    privateID = "" -- for the future
    tokenKey = tokenize sk $ keyToBS key

-- | Hash session info into a HMAC
hash ∷ ServerKey → Token → PrivateID → ByteString → HMAC SHA3_224
hash sk tokenKey privateID sesData = hmac sk msg
    where msg = BS8.intercalate
            (BS8.singleton sesInnerSep)
            [tokenKey, sesData, privateID]

keyToBS ∷ PersistEntity record ⇒ Key record → ByteString
keyToBS = BSL8.toStrict . encode

-- | Convert 'Session' into 'ByteString' to (probably) form a cookie
toCookieValue ∷ (PersistEntity record) ⇒ Session record → BS8.ByteString
toCookieValue Session{sesKey, sesToken, sesData, sesHMAC} =
        BS8.intercalate (BS8.singleton sesInnerSep)
            [ sesToken
            , sesData
            , BS8.pack $ show $ hmacGetDigest $ sesHMAC
            ]

-- | Parse a cookie value into 'Session's to then retrieve the users key.
parseSessions ∷ ByteString → [[ByteString]]
parseSessions sessions = sessions
    & BS8.split sesOuterSep
    & fmap (BS8.split sesInnerSep)

fromAuthorization
    ∷ (PersistEntity record)
    ⇒ ServerKey → [HTTP.Header] → Maybe (Session record)
fromAuthorization sk headers = do
    auth ← lookup "Authorization" headers
    (user,session) ← extractBasicAuth auth
    validateSession sk session

-- | Convert a http '[Header]' into a 'Session' type.
fromRequest
    ∷ (PersistEntity record)
    ⇒ ServerKey → [HTTP.Header] → Maybe (Session record)
fromRequest sk headers = do
    cookies ← parseCookies <$> lookup hCookie headers
    tokenSesKey ← lookup userField cookies
    sessions ← lookup sesField cookies

    session ← sessions
            & BS8.split sesOuterSep
            & find (matchingKey tokenSesKey)
    validateSession sk session
  where
    matchingKey tokenSesKey ses =
        case BS8.split sesInnerSep ses of
            (key:_) → key == tokenSesKey
            _ → False

validateSession
    ∷ (PersistEntity record)
    ⇒ ServerKey → BS8.ByteString → Maybe (Session record)
validateSession sk session = do
    [key,sesData,truth] ← pure $ BS8.split sesInnerSep session
    validKey ← decode $ BSL8.fromStrict $ untokenize sk key
    let session' = toSession sk validKey sesData
    guard $ isValid truth session'
    pure session'
  where
    isValid ∷ ByteString → Session record → Bool
    isValid truth session' = truth == BS8.pack (show $ hmacGetDigest $ sesHMAC session')

validateHeaders
    ∷ (PersistEntity record)
    ⇒ ServerKey → [HTTP.Header] → [Session record]
validateHeaders sk headers = catMaybes
    [ fromAuthorization sk headers
    , fromRequest sk headers
    ]

updateSession ∷ Token → ByteString → ByteString → ByteString
updateSession sesToken ses acc =
    case BS8.split sesInnerSep ses of
      (cookieKey:_) → if cookieKey == sesToken
                         then acc
                         else intercalate' [ses, acc]
      _ → acc
  where
    intercalate' = BS8.intercalate $ BS8.singleton sesOuterSep
