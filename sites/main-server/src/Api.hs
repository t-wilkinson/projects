module Api where

import Quaalude
import Core.App (AppT(..), AppS)
import Core.Config (Config, Environment(..), env)
import qualified Booth.Api as Booth
import qualified Dogwalking.Api as Dogwalking

import Network.Wai.Middleware.Cors          (CorsResourcePolicy(..), simpleCorsResourcePolicy , cors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant.Server.Generic               (genericServeTWithContext)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T

instance FromHttpApiData ByteString where
    parseQueryParam = Right . BS8.pack . T.unpack

data Route r = Route
    { _booth ∷ r :- "booth" :> ToServantApi Booth.Route
    , _dogwalking ∷ r :- "dogwalking" :> ToServantApi Dogwalking.Route
    } deriving (Generic)

route ∷ Route AppS
route = Route
    (toServant Booth.route)
    (toServant Dogwalking.route)

type Api = Proxy (ToServantApi Route)

api ∷ Api
api = genericApi $ Proxy @Route

-- | App serving a context
app ∷ Config → Application
app cfg = case cfg^.env of
    Development → cors (const $ Just policy)
        $ logStdoutDev
        $ genericServeTWithContext nt Api.route ctx
    Production → cors (const $ Just policy)
        $ genericServeTWithContext nt Api.route ctx
    Remote → cors (const $ Just policy)
        $ logStdoutDev
        $ genericServeTWithContext nt Api.route ctx
    where
        nt ∷ ∀ a. AppT Config IO a → Handler a
        nt appt = Handler $ runReaderT (runAppT appt) cfg
        ctx =
               Dogwalking.maybeUserAuthHandler cfg
            :. Dogwalking.userAuthHandler cfg
            :. Booth.maybeUserAuthHandler cfg
            :. Booth.userAuthHandler cfg
            :. Booth.stripeHookHandler cfg
            :. Booth.adminAuthHandler cfg
            :. EmptyContext
        policy = simpleCorsResourcePolicy
                { corsOrigins = Just (
                    [ "http://localhost"
                    , "http://localhost:3000"
                    , "https://kleanstudio.com"
                    , "https://pawpals.treywilkinson.com"
                    , "https://treywilkinson.com"
                    ], True)
                , corsMethods        = [ "DELETE", "PUT", "POST", "GET", "OPTIONS"]
                , corsExposedHeaders = Just
                    [ "Set-Cookie"
                    , "Access-Control-Allow-Origin"
                    , "Access-Control-Allow-Credentials"
                    ]
                , corsRequestHeaders =
                    [ "Content-Type"
                    , "Access-Control-Request-Method"
                    , "Access-Control-Request-Headers"
                    , "Authorization"
                    ]
                }
