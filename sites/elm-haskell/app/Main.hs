module Main where


-- General
import Control.Monad.IO.Class

-- Custom Libraries
import qualified Blog.Api
import qualified Blog.Database 
import qualified Elders.Api
import qualified Elders.Database

-- import Lib ( API, Databases )

-- Api
import Servant 

-- Network
import Network.Wai
import Network.Wai.Handler.Warp ( setPort, run, defaultSettings )
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Handler.WarpTLS

-- Database
import Data.Acid ( openLocalStateFrom )

import Data.Text
import Servant.API 


type Databases =
    ( Blog.Database.DB
    , Elders.Database.DB
    )


type API =
         Blog.Api.API
    :<|> Elders.Api.API


server :: Databases -> Server API
server (blogsdb,eldersdb) =
         Blog.Api.server blogsdb
    :<|> Elders.Api.server eldersdb


api :: Proxy API
api = Proxy


app :: Databases -> Application
app databases = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions api 
    $ serve api (server databases)
  where
    policy = simpleCorsResourcePolicy 
        { corsMethods = ["DELETE"]
        , corsRequireOrigin = False
        , corsRequestHeaders = [ "content-type" ] 
        } 


main :: IO ()
main = do
    databases <- (,) 
        <$> Blog.Api.database 
        <*> Elders.Database.openDatabase

    runTLS tlsOpts warpOpts (app databases)

    where
        tlsOpts = tlsSettings "/home/trey/brain/projects/sites/app/fullchain.pem" "/home/trey/brain/projects/sites/app/privkey.pem"
        warpOpts = setPort 8080 defaultSettings
    --run 8080 $ app databases


--myTls tset set app =
--    withSocketsDo $ 
--        bracket
--            (bindPortTCP (getPort set) (getHost set))
--            close
--            (\sock -> runTLSSocket tset set sock app)
