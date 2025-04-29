
module Main where

import Quaalude
import Api (app, api)
import Core.Config ( Config(..), Environment(..), port, pool, env)
import Db
import Core.Db (migrateAll)
import Servant.JS
import Servant.JS.Vanilla

import Control.Exception (bracket)
import Network.Wai.Handler.Warp ( Settings
             , runSettings , defaultSettings , setPort
             , setOnExceptionResponse , exceptionResponseForDebug
             )
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql (createPostgresqlPool, runSqlPool, runMigration, migrate, entityDef, runMigrationUnsafe, rawExecute)

import MyCookie (AuthCookieSettings(..), mkPersistentServerKey, mkRandomSource)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (ctrCombine)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Random (drgNew)
import Control.Monad.Logger (LoggingT(..), Loc(..), MonadLogger(..), toLogStr, defaultLogStr)

import System.IO (IOMode(..), withFile, hPrint, hPutStrLn)
import qualified Data.Text.IO as TextIO

instance MonadLogger IO  where
    monadLoggerLog _ _ _ msg = do
        let str = show $ toLogStr msg
        -- log date/time as well
        withFile "../db.log" AppendMode (\h -> hPutStrLn h str)

-- | Run our Server
main :: IO ()
main = do
    TextIO.writeFile "../test.js" $ jsForAPI api vanillaJS
    bracket initConfig shutdown runApp

-- | Allocate resources for database
initConfig :: IO Config
initConfig = do
    pool <- createPostgresqlPool connStr 1
    runSqlPool (runMigrationUnsafe migrateAll) pool

    randomSource <- mkRandomSource drgNew 2000
    putStrLn $ "serving port: " <> show port
    pure $ Config port env pool acs sks randomSource ss
  where
    connStr = "host=localhost dbname=test user=test password=test port=5432"
    port = 8001
    env  = Development
    sks = mkPersistentServerKey "thisisakey"
    ss = def
    acs = AuthCookieSettings
        { acsSessionField     = "session"
        , acsCookieFlags      = ["HttpOnly"]
        , acsMaxAge           = fromIntegral (6 * 3600 :: Integer)
        , acsPath             = "/"
        , acsHashAlgorithm    = Proxy @SHA256
        , acsCipher           = Proxy @AES256
        , acsEncryptAlgorithm = ctrCombine
        , acsDecryptAlgorithm = ctrCombine
        }

-- | Shutdown Server
shutdown :: Config -> IO ()
shutdown cfg = do
    traverse dropDb ["\"user\"", "rent_request"]
    destroyAllResources (cfg^.pool)
  where
      dropDb table = runSqlPool (rawExecute ("DROP TABLE " <> table) []) (cfg^.pool)

-- | Run the app, generating elm when in dev mode
runApp :: Config -> IO ()
runApp config = do
    let app' = app config
    case config^.env of
      _ -> runSettings (initSettings config) app'

-- | Set default server settings such as port
initSettings :: Config -> Settings
initSettings config = setOnExceptionResponse exceptionResponseForDebug
      $ setPort (config^.port) defaultSettings

