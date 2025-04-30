module Core.Helpers where

import Quaalude
import Core.Config (Config, ServerKey, manager)
import Core.Session (Session(..), updateSession, toCookieValue, userField, sesField, sesOuterSep)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Value)
import Network.HTTP.Client (Manager, Request(..), Response(..), RequestBody(..))
import Network.HTTP.Types.URI (urlEncodeBuilder)
import Web.Cookie
import Data.ByteString.Char8 (ByteString)

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Database.Esqueleto as E
import qualified Core.Errors as CE
import qualified Data.Aeson as JSON

getMatchingRecordsByField
  ∷ ∀ val typ.
    ( E.PersistEntity val
    , E.BackendCompatible E.SqlBackend (E.PersistEntityBackend val)
    , E.PersistField typ
    )
  ⇒ E.EntityField val typ
  → typ
  → E.SqlPersistT IO [E.Entity val]
getMatchingRecordsByField field typ = do
    E.select $ E.from $
      \entity → do
          E.where_ (entity E.^. field E.==. E.val typ)
          pure entity

{- Cookies -}
-- | Create a session cookie
addSessionUserCookie
    ∷ ∀ entity value.
        (E.PersistEntity entity)
    ⇒ Session entity
    → Maybe ByteString
    → value
    → Headers '[ Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie] value
addSessionUserCookie session maybeCookies value = do
    addCookie session maybeCookies baseCookie value

-- | Create a persistent cookie
addUserCookie
    ∷ ∀ entity value.
        (E.PersistEntity entity)
    ⇒ Session entity
    → Maybe ByteString
    → value
    → Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] value
addUserCookie session maybeCookies value =
    addCookie session maybeCookies createCookie value

createCookie ∷ ByteString → ByteString → SetCookie
createCookie name value = (baseCookie name value)
    { setCookieMaxAge = Just $ secondsToDiffTime $ 365 * 24 * 60 * 60
    }

persistCookie ∷ ByteString → ByteString → SetCookie
persistCookie name value = (baseCookie name value)
    { setCookieMaxAge = Just $ secondsToDiffTime $ 365 * 24 * 60 * 60
    }

sessionCookie ∷ ByteString → ByteString → SetCookie
sessionCookie name value = (baseCookie name value)
    { setCookieMaxAge = Nothing
    }

baseCookie ∷ ByteString → ByteString → SetCookie
baseCookie name value = def
    { setCookieName = name
    , setCookieValue = value
    , setCookieSameSite = Just sameSiteNone -- TODO sameSiteLax
    , setCookiePath = Just "/"
    , setCookieSecure = True
    , setCookieHttpOnly = True
    }

-- | Low level function for generating header cookies
addCookie
    ∷ ∀ entity a.
        (E.PersistEntity entity)
    ⇒ Session entity
    → Maybe ByteString
    → (ByteString → ByteString → SetCookie)
    → a
    → Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] a
addCookie session maybeCookies makeCookie value = do
    let sessionToken = sesToken session
        sessionCookie' = toCookieValue session
        maybeSessions = maybeCookies >>= lookup "sessions" . parseCookies

    case maybeSessions of
      Nothing → addHeaders sessionCookie' sessionToken
      (Just sessions) →
          let sessions' = foldr (updateSession sessionToken) sessionCookie'
                $ BS8.split sesOuterSep sessions
          in
          addHeaders sessions' sessionToken
  where
    addHeaders v1 v2 =
        addHeader (makeCookie sesField v1)
        $ addHeader (makeCookie userField v2)
        $ value

{-- Requests --}
type Req a = ServerKey → [(ByteString, String)] → StdMethod → [String] → a
{- Stripe Requests -}
parseStripeRes ∷ (MonadError ServerError m, FromJSON a) ⇒ Maybe Value → m a
parseStripeRes mayValue = maybe (trace "Error parsing stripe" $ throwError CE.stripeUser) (pure) $ mayFromJSON mayValue

stripeReqBS
    ∷   ( MonadReader Config m
        , MonadIO m
        )
    ⇒ Req
    (m (BSL8.ByteString))
stripeReqBS key body method paths = do
    manager' ← asks (^.manager)
    liftIO $ fmap (responseBody) $ stripeReq' "https://api.stripe.com" key body method paths manager'

stripeReq
    ∷   ( MonadReader Config m
        , MonadIO m
        )
    ⇒ Req
    (m (Maybe Value))
stripeReq key body method paths = do
    manager' ← asks (^.manager)
    liftIO $ fmap (decode . responseBody) $ stripeReq' "https://api.stripe.com" key body method paths manager'

stripeReq'
    ∷ Request
    → Req
    (Manager → IO (Response BSL8.ByteString))
stripeReq' origin key body method paths manager' =
    (httpLbs origin
        { method = BS8.pack $ show method
        , path = BS8.pack $ intercalate "/" paths
        , requestHeaders = stripeHeaders key
        , requestBody = RequestBodyLBS $ urlEncode body
        } manager')
  where
    stripeHeaders ∷ ByteString → [(CI ByteString, ByteString)]
    stripeHeaders key' =
        [ ("Content-Type", "application/x-www-form-urlencoded")
        , ("Authorization", "Bearer " <> key')
        ]

urlEncode ∷ [(ByteString, String)] → BSL8.ByteString
urlEncode = Builder.toLazyByteString . mconcat . intersperse (Builder.shortByteString "&") . fmap encodePair
  where
    escape = urlEncodeBuilder True
    encodePair (k, "") = escape k
    encodePair (k, v)  = escape k <> Builder.shortByteString "=" <> escape (BS8.pack v)

{- Misc -}
prettyPrintReq ∷ Maybe Value → IO ()
prettyPrintReq = liftIO . putStrLn . fmap (toEnum.fromEnum) . BSL8.unpack . encodePretty

prettyPrintBody ∷ BS8.ByteString → IO ()
prettyPrintBody body =
    case (decode (BSL8.fromStrict body) :: Maybe JSON.Value) of
        Just value → liftIO $ BSL8.putStrLn $ encodePretty value
        Nothing → pure ()

mayFromJSON ∷ (FromJSON a) ⇒ Maybe Value → Maybe a
mayFromJSON mayValue = do
    value ← mayValue
    case fromJSON value of
        JSON.Success a → Just a
        JSON.Error _ → Nothing
