{-# LANGUAGE RankNTypes #-}

-- | This is the center of api

module Api where

import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import qualified Api.Beat   as   Beat
import qualified Api.User   as   User
import qualified Api.Studio   as   Studio
import qualified Api.Secret as Secret

import Config ( App, AppS, AppT(..), Config )
import Servant.Server ( Context (..) )
import Servant.Server.Generic ( genericServeTWithContext )
import Servant ( Application, Handler )
import Control.Natural ( type (~>) )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Network.Wai.Middleware.Cors ( CorsResourcePolicy(..)
                                                , simpleCorsResourcePolicy
                                                , cors
                                                )
-- import           Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant.API.Generic ( toServant )


data Route route = Route
    { users :: route
        :- "user"
        :> ToServant User.Route AsApi

    , studio :: route
        :- "studio"
        :> ToServant Studio.Route AsApi

    , secrets :: route
        :- ToServant Secret.Route AsApi
    } deriving Generic


-- | Combine all servers
route :: Route AppS
route = Route
    { users = toServant User.route
    , studio = toServant Studio.route
    , secrets = toServant Secret.route
    }

-- | App serving a context
app :: Config -> Application
app cfg = cors (const $ Just policy)
    $ genericServeTWithContext nt Secret.route ctx
  where
    nt :: App ~> Handler
    nt appt = runReaderT (runAppT appt) cfg
    ctx = Secret.authHandler :. EmptyContext
    policy = simpleCorsResourcePolicy
        { corsMethods        = [ "DELETE", "PUT" ]
        , corsRequireOrigin  = False
        , corsRequestHeaders = [ "content-type" , "Access-Control-Allow-Origin" ]
        }


data ElmRoute route = ElmRoute
    { _users :: route
        :- "user"
        :> ToServant User.Route AsApi
    , _studio :: route
        :- "studio"
        :> ToServant Studio.Route AsApi
    } deriving Generic

type Api = ToServant ElmRoute AsApi

-- api = genericApi (Proxy @Route)

-- | Serve application, passing config to server
{-
app cfg = cors (const $ Just policy)
  -- . provideOptions api
  . serve api
  . appToServer
  $ cfg
-}

-- | Allow us to run Servant with custom datatype
-- appToServer :: Config -> Server Api
-- appToServer cfg = hoistServer api (nt cfg) (toServant route)
