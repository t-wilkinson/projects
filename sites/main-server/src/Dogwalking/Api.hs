module Dogwalking.Api where

import Quaalude
import Core.App (AppS)
import Core.Config (Config, dogwalking, sk, pool)
import Dogwalking.Db
import Core.Session (Session(..), validateHeaders)
import qualified Dogwalking.Api.Session as Session
import qualified Dogwalking.Api.Service as Service
import qualified Dogwalking.Api.ServiceProtected as ServiceProtected
import qualified Core.Errors as CE

import Database.Persist.Sql (runSqlPool)
import Network.Wai (Request(..))
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import qualified Data.ByteString.Char8 as BS8
import qualified Database.Esqueleto as E

type instance AuthServerData (AuthProtect "dogwalking-user") = E.Entity User
type instance AuthServerData (AuthProtect "dogwalking-maybe-user") = Maybe (E.Entity User)

type Api = Proxy (ToServantApi Route)

api ∷ Api
api = genericApi $ Proxy @Route

data Route r = Route
    { _session ∷ r
        :- "session"
        :> ToServantApi Session.Route
    , _service ∷ r
        :- "service"
        :> Header' [Strict, Required] "Authorization" BS8.ByteString
        :> AuthProtect "dogwalking-maybe-user"
        :> ToServantApi Service.Route
    , _serviceProtected ∷ r
        :- "service"
        :> Description "Manipulation of existing services."
        :> Header' [Strict, Required] "Authorization" BS8.ByteString
        :> AuthProtect "dogwalking-user"
        :> ToServantApi ServiceProtected.Route
    } deriving (Generic)

route ∷ Route AppS
route = Route
    (toServant $ Session.route)
    (\_ x → toServant $ Service.route x)
    (\_ x → toServant $ ServiceProtected.route x)

userAuthHandler ∷ Config → AuthHandler Request (E.Entity User)
userAuthHandler cfg = mkAuthHandler \req → do
    case validateHeaders (cfg^.sk) (requestHeaders req) of
        [] → throwError CE.invalidSession
        (session:_) → do
            let key = sesKey session
            maybeUser ← liftIO $ runSqlPool (E.get key) (cfg^.dogwalking.pool)
            maybe (throwError CE.userDNE) (pure . E.Entity key) maybeUser

maybeUserAuthHandler ∷ Config → AuthHandler Request (Maybe (E.Entity User))
maybeUserAuthHandler cfg = mkAuthHandler \req → do
    case validateHeaders (cfg^.sk) (requestHeaders req) of
        [] → pure $ Nothing
        (session:_) → do
            let key = sesKey session
            maybeUser ← liftIO $ runSqlPool (E.get key) (cfg^.dogwalking.pool)
            pure $ fmap (E.Entity key) maybeUser
