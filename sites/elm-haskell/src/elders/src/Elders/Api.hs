module Elders.Api where


-- Time
import Data.Time.Clock
import Data.Time.Calendar

-- General
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)

-- API
import Servant.API 
import Servant 
import Servant.Server.StaticFiles

-- Network
import Network.HTTP.Types  ( status404, status200 )
import Network.Wai
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Servant.Options

-- Database
import Data.Acid 
import Data.IntMap (IntMap, Key)
import Data.Typeable
import Elders.Database hiding ( getPeople, getDays, postPeople )
import qualified Data.IntMap as IntMap

-- Json
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Aeson
import Data.Maybe
import Debug.Trace
import System.IO


---- Api ----


type API =   
             "people" :> Get '[JSON] People
        :<|> "people" :> ReqBody '[JSON] [Person] :> Post '[JSON] ()
        :<|> "days" :> Get '[JSON] Days


api :: Proxy API
api = Proxy


app :: DaysDB -> Application
app db = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions api
    $ serve api (server db)
  where
    policy = simpleCorsResourcePolicy
        { corsMethods = ["DELETE"]
        , corsRequireOrigin = False
        , corsRequestHeaders = [ "content-type" ]
        }

-- app :: DaysDB -> Application
-- app db = serve api (server db)



---- Server ----


server :: DaysDB -> Server API
server daysdb = 
         getPeople  
    :<|> postPeople daysdb 
    :<|> getDays daysdb


getPeople :: Handler People
getPeople = do
    contents <- liftIO $ B.readFile "../database/quorum.json" 
    return $ fromMaybe (People [Person {name="bob", icon="http"}]) . decode $ contents


postPeople :: DaysDB -> [Person] -> Handler ()
postPeople db ps = do
    time <- liftIO $ toGregorian . utctDay <$> getCurrentTime
    liftIO $ update db (PostPeople time ps)


getDays :: DaysDB -> Handler Days
getDays db = liftIO $ query db GetDays


