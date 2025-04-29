{-# Language
  TemplateHaskell
#-}

module Api where

import Quaalude
import Core.App (AppT(..), AppS)
import Core.Config ( Config, authSettings, serverKeySet)
import qualified Api.User as User
import qualified Api.Session as Session

import Network.Wai (Request, Middleware, getRequestBodyChunk, requestHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..) , simpleCorsResourcePolicy , cors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Servant.API (AuthProtect)
import Servant.API.Generic (AsApi, ToServant, toServant, ToServantApi, genericApi)
import Servant.Foreign (HasForeign(..), HasForeignType)
import Servant.Server (Context (..), Handler, Application)
import Servant.Server.Experimental.Auth (mkAuthHandler, AuthHandler, AuthServerData)
import Servant.Server.Generic (genericServeTWithContext)

import GHC.TypeLits (KnownSymbol)

type instance AuthServerData (AuthProtect "cookie") = Maybe String

instance (KnownSymbol sym, HasForeign lang ftype sub)
  => HasForeign lang ftype (AuthProtect sym :> sub) where
    type Foreign ftype (AuthProtect sym :> sub) = Foreign ftype sub
    foreignFor lang ftype _ req = foreignFor lang ftype (Proxy @sub) req

data Route r = Route
    { _user :: r
        :- "user"
        :> AuthProtect "cookie"
        :> ToServant User.Route AsApi
    , _session :: r
        :- ToServant Session.Route AsApi
    } deriving (Generic)

route = Route @AppS
    -- (toServant User.route)
    (\s -> (toServant User.route))
    (toServant Session.route)

api :: Proxy (ToServantApi Route)
api = genericApi $ Proxy @Route

-- | App serving a context
app :: Config -> Application
app cfg = cors (const $ Just policy)
    $ logStdoutDev
    $ genericServeTWithContext nt Api.route ctx
  where
    nt :: forall a. AppT Config Handler a -> Handler a
    nt appt = runReaderT (runAppT appt) cfg
    ctx = User.cookieAuthCheck cfg :. EmptyContext
    policy = simpleCorsResourcePolicy
        { corsMethods        = [ "DELETE", "PUT", "POST", "GET", "OPTIONS"]
        , corsRequireOrigin  = False
        , corsOrigins = Nothing
        , corsRequestHeaders =
            [ "Content-Type"
            , "Access-Control-Allow-Origin"
            ]
        }

