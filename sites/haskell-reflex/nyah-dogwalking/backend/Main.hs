import Quaalude
import Api ( app )
import Db ()
import Core.Config ( Config(..), Environment(..), port, pool, env)

import Control.Exception ( bracket )
import Network.Wai.Handler.Warp ( Settings
             , runSettings , defaultSettings , setPort
             , setOnExceptionResponse , exceptionResponseForDebug
             )
import Database.Selda.SQLite ( sqliteOpen, seldaClose )
import Data.Pool ( createPool, destroyAllResources )
-- import Network.Wai.Handler.WarpTLS ( runTLS, tlsSettings )
-- import Network.Wai.Handler.Warp (run)
-- import Database.Selda.PostgreSQL ( pgOpen, seldaClose, on )

-- | Run our Server
main :: IO ()
main = bracket initConfig shutdown runApp

-- | Allocate resources for database
initConfig :: IO Config
initConfig = do
    let port = 8000
        env  = Development
    pool <- createPostgresqlPool
    putStrLn $ "serving port: " <> show port
    pure $ Config port env pool
  where
      createPostgresqlPool = createPool
        -- (pgOpen ("test" `on` "127.0.0.1")) -- ^ postgresql
        (sqliteOpen "db.sqlite") -- ^ sqlite
        seldaClose
        1  -- sub-pools to maintain
        1  -- time an unused resource is kept open
        1  -- number of resources to keep open per stripe

-- | Shutdown Server
shutdown :: Config -> IO ()
shutdown config = destroyAllResources (config^.pool)

-- | Run the app, generating elm when in dev mode
runApp :: Config -> IO ()
runApp config = do
    let app' = app config
    case config^.env of
      _ -> runSettings (initSettings config) app'
      -- _ -> runTLS (tlsSettings "priv/cert.pem" "priv/secret-key.pem")
      --             (initSettings config)
      --             app'

-- | Set default server settings such as port
initSettings :: Config -> Settings
initSettings config = setOnExceptionResponse exceptionResponseForDebug
      $ setPort (config^.port) defaultSettings

