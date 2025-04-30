-- | Initialize app

module Init where

import           Api                            ( app )
import           Config                         ( Config(..)
                                                , Environment(..)
                                                , pool
                                                , port
                                                , env
                                                )
import GenerateElm    ( elmGen )

import           Control.Exception              ( bracket )
import           System.Environment             ( lookupEnv )
import           Data.Pool                      ( destroyAllResources )
import           Database.Persist.Postgresql    ( createPostgresqlPool
                                                , runSqlPool
                                                )
import           Network.Wai.Handler.Warp       ( Settings(..)
                                                , run
                                                , runSettings
                                                , defaultSettings
                                                , setPort
                                                , setOnExceptionResponse
                                                , exceptionResponseForDebug
                                                )
import           Network.Wai.Handler.WarpTLS    ( runTLS
                                                , tlsSettings
                                                )


-- | Run our Server
runServer :: IO ()
runServer = bracket initConfig shutdown runApp

-- | Allocate resources for database
initConfig :: IO Config
initConfig = do
  let port = 8080
      env  = Development
  pool <- createPostgresqlPool connStr 1
  -- TODO read info from json file
  pure $ Config port pool env
 where
  connStr = "host=localhost dbname=test user=test password=test port=5432"

-- | Shutdown Server
shutdown :: Config -> IO ()
shutdown config = destroyAllResources (config ^. pool)

-- | Run the app, generating elm when in dev mode
runApp :: Config -> IO ()
runApp config = do
  app <- pure (app config)
  case config ^. env of
    Development -> do
      elmGen config
      runSettings (initSettings config) app
    _ -> runTLS (tlsSettings "priv/cert.pem" "priv/secret-key.pem")
                (initSettings config)
                app

-- | Set default server settings such as port
initSettings :: Config -> Settings
initSettings config =
  setOnExceptionResponse exceptionResponseForDebug
    $ setPort (config ^. port) defaultSettings
