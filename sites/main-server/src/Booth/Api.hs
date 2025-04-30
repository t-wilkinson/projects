{-# language TypeApplications #-}
module Booth.Api where

-- Local
import Quaalude
import Core.Config (Environment(..), Config, booth, sk, pool, adminEmail, env, stripeTestHook, stripeLiveHook)
import Core.App (AppS)
import Booth.Db
import Core.Session (Session(..), validateHeaders)
import qualified Booth.Api.Projects as Projects
import qualified Booth.Api.Session as Session
import qualified Booth.Api.Service as Service
import qualified Booth.Api.Payment as Payment
import qualified Booth.Api.ServiceProtected as ServiceProtected
import qualified Booth.Api.Hooks as Hooks
import qualified Booth.Api.Admin as Admin
import qualified Core.Errors as CE

-- Outside
import Database.Persist.Sql (runSqlPool)
import Network.Wai (Request(..), strictRequestBody)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16

type instance AuthServerData (AuthProtect "booth-user") = E.Entity User
type instance AuthServerData (AuthProtect "maybe-booth-user") = (Maybe (E.Entity User), Rates)
type instance AuthServerData (AuthProtect "stripe-hooks") = (Request, BS8.ByteString)
type instance AuthServerData (AuthProtect "booth-admin") = ()

type Api = Proxy (ToServantApi Route)

api ∷ Api
api = genericApi $ Proxy @Route

data Route r = Route
    { _session ∷ r
        :- "session"
        :> Description "Sessions for users"
        :> ToServantApi Session.Route
    , _service ∷ r
        :- "service"
        :> Description
            "Services have a start+end time, duration, and are used to calculate user charge.\
            \Can get services and designate which ones are owned."
        :> Header' [Strict, Required] "Authorization" BS8.ByteString
        :> AuthProtect "maybe-booth-user"
        :> ToServantApi Service.Route
    , _serviceProtected ∷ r
        :- "service"
        :> Description "Manipulation of existing services."
        :> Header' [Strict, Required] "Authorization" BS8.ByteString
        :> AuthProtect "booth-user"
        :> ToServantApi ServiceProtected.Route
    , _payment ∷ r
        :- "payment"
        :> Description "Stripe payment methods and payment intents."
        :> Header' [Strict, Required] "Authorization" BS8.ByteString
        :> AuthProtect "booth-user"
        :> ToServantApi Payment.Route
    , _hooks ∷ r
        :- "hooks"
        :> Description "Handle stripe hooks, passing the entire 'Request' through"
        :> AuthProtect "stripe-hooks"
        :> ToServantApi Hooks.Route
    , _admin ∷ r
        :- "admin"
        :> AuthProtect "booth-admin"
        :> ToServantApi Admin.Route
    , _projects ∷ r
        :- "projects"
        :> ToServantApi Projects.Route
    } deriving (Generic)

route ∷ Route AppS
route = Route
    (toServant $ Session.route)
    (\_ x → toServant $ Service.route x)
    (\_ x → toServant $ ServiceProtected.route x)
    (\_ x → toServant $ Payment.route x)
    (\x → toServant $ Hooks.route x)
    (\_ → toServant $ Admin.route)
    (toServant $ Projects.route)

adminAuthHandler ∷ Config → AuthHandler Request ()
adminAuthHandler cfg = mkAuthHandler \req → do
    case (cfg^.env) of
        Development → pure ()
        Remote → validateAdmin req
        Production → validateAdmin req

    where validateAdmin req = case validateHeaders (cfg^.sk) (requestHeaders req) of
            [] → throwError CE.invalidSession
            (session:_) → do
                let key = sesKey session
                maybeUser ← liftIO $ runSqlPool (E.get key) (cfg^.booth.pool)
                user ← maybe (throwError CE.userDNE) (pure) maybeUser
                if (cfg^.booth.adminEmail) == userEmail user
                    then pure ()
                    else throwError CE.notAdmin

userAuthHandler ∷ Config → AuthHandler Request (E.Entity User)
userAuthHandler cfg = mkAuthHandler \req → do
    case validateHeaders (cfg^.sk) (requestHeaders req) of
        [] → throwError CE.invalidSession
        (session:_) → do
            let key = sesKey session
            maybeUser ← liftIO $ runSqlPool (E.get key) (cfg^.booth.pool)
            maybe (throwError CE.userDNE) (pure . E.Entity key) maybeUser

maybeUserAuthHandler ∷ Config → AuthHandler Request (Maybe (E.Entity User), Rates)
maybeUserAuthHandler cfg = mkAuthHandler \req → do
    mayRates ∷ Maybe (P.Entity Rates) ← liftIO $ runSqlPool (P.selectFirst [] []) (cfg^.booth.pool)
    rates ← maybe (throwError CE.noRates) (pure . E.entityVal) mayRates
    case validateHeaders (cfg^.sk) (requestHeaders req) of
        [] → pure $ (Nothing, rates)
        (session:_) → do
            let key = sesKey session
            maybeUser ← liftIO $ runSqlPool (E.get key) (cfg^.booth.pool)
            pure $ (fmap (E.Entity key) maybeUser, rates)

stripeHookHandler ∷ Config → AuthHandler Request (Request, BS8.ByteString)
stripeHookHandler cfg = mkAuthHandler \req → do
    -- Get header information
    liftIO $ putStrLn "stripe hook handler - start"
    let headers = requestHeaders req
        testKey = (cfg^.booth.stripeTestHook)
        liveKey = (cfg^.booth.stripeLiveHook)
        mayExtracted = do
            sig ← lookup "Stripe-Signature" headers
            let kvs = mapMaybe keyValuePair $ BS8.split ',' sig
            v1 ← lookup "v1" kvs
            t ← lookup "t" kvs
            pure (v1,t)
    (stripe_v1,stripe_t) ← maybe (throwError CE.parseRequest) pure mayExtracted

    -- Validate 'Stripe-Signature'
    body ← liftIO $ BSL8.toStrict <$> strictRequestBody req
    let signedPayload = stripe_t <> "." <> body
        key = case cfg^.env of
            Development → testKey
            Remote → testKey
            Production → liveKey
        digest = Crypto.hmacGetDigest $ Crypto.hmac key signedPayload ∷ Crypto.Digest Crypto.SHA256
        hashedPayload = Base16.encode $ BS.pack $ BA.unpack $ digest
    when (hashedPayload /= stripe_v1) $ throwError CE.stripeSignature
    liftIO $ putStrLn "stripe hook handler - end"
    pure (req, body)
  where
    keyValuePair kv = case BS8.split '=' kv of
                       (k:v:[]) → Just (k,v)
                       _ → Nothing
