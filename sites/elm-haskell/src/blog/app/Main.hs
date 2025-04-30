module Main where

import Blog.Database (DB)
import Blog.Api (API, server, database)
import Network.Wai
import Network.Wai.Handler.Warp ( setPort, run, defaultSettings )
import Network.Wai.Middleware.Cors 
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Network.Wai.Middleware.Servant.Options
import Network.Wai.Handler.WarpTLS
import Servant 


api :: Proxy API
api = Proxy

app :: DB -> Application
app database = logStdoutDev
    $ cors (const $ Just policy)
    $ provideOptions api 
    $ serve api (server database)
  where
    policy = simpleCorsResourcePolicy 
        { corsMethods = ["DELETE"]
        , corsRequireOrigin = False
        , corsRequestHeaders = [ "content-type" ] 
        } 


main :: IO ()
main = do
    db <- database
    runTLS tlsOpts warpOpts (app db)

    where
        tlsOpts = tlsSettings "../../app/fullchain.pem" "../../app/privkey.pem"
        warpOpts = setPort 8000 defaultSettings

