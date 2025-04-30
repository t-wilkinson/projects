-- {-# LANGUAGE
-- #-}

module Api where

import Quaalude
import Core.App ( AppT(..), AppS )
import Core.Config ( Config(..) )
import qualified Api.Secret as   Secret
import qualified Api.User   as   User

import Control.Natural ( type (~>) )
import Network.Wai ( Middleware, getRequestBodyChunk, requestHeaders )
import Network.Wai.Middleware.Cors ( CorsResourcePolicy(..)
  , simpleCorsResourcePolicy , cors)
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant.API ( Raw )
import Servant.API.Generic ( AsApi, ToServant, toServant )
import Servant.Server ( Context (..), Handler, Application )
import Servant.Server.Generic ( genericServeTWithContext )
import Servant.Server.StaticFiles (serveDirectoryFileServer)

type (:>>) a b = a b

-- type level programming?
data Route r = Route
    { user :: r
        :- "user"
        :> ToServant User.Route AsApi
    , private :: r
        :- "private"
        :> ToServant Secret.Route AsApi
    , raw :: r
        :- Raw
    } deriving Generic

route = Route @AppS
    (toServant User.route)
    (toServant Secret.route)
    (serveDirectoryFileServer "/home/trey/reading")

-- | App serving a context
app :: Config -> Application
app cfg = cors (const $ Just policy)
    $ logStdoutDev
    $ genericServeTWithContext nt Api.route ctx
  where
    nt :: AppT Config Handler ~> Handler
    nt appt = runReaderT (runAppT appt) cfg

    ctx = Secret.authHandler :. EmptyContext

    policy = simpleCorsResourcePolicy
        { corsMethods        = [ "DELETE", "PUT", "POST", "GET" ]
        , corsRequireOrigin  = False
        , corsRequestHeaders =
            [ "Content-Type"
            , "Access-Control-Allow-Origin"
            ]
        }

