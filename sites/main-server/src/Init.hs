
{-# OPTIONS_GHC -Wno-all #-}

{-# LANGUAGE TemplateHaskell #-}
module Init where

import Quaalude
import Core.Config (Config(..), Environment(..), Client(..), ClientName(..), port, env, booth, pool)
import qualified Core.Config
import qualified Booth.Db
import qualified Dogwalking.Db
import qualified Api
-- import qualified Booth.Db.Types as Booth.Db
import Booth.Db.Types

import qualified Booth.Api.Service as BService
import qualified Booth.Api.Payment as BPayment
import qualified Booth.Api.ServiceProtected as BServiceProtected
import qualified Booth.Api.Session as BSession

import Control.Monad.Logger        (MonadLogger(..), toLogStr, fromLogStr)
import Data.Pool                   (destroyAllResources)
import Database.Persist.Postgresql (ConnectionPool, Migration, createPostgresqlPool, runSqlPool, runMigrationUnsafe)
import Network.Wai.Handler.Warp    (Settings, runSettings, defaultSettings, setPort, setHost)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Crypto.KDF.BCrypt  (hashPassword)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Database.Persist as P
import qualified Database.Esqueleto as E

instance MonadLogger IO  where
    monadLoggerLog _ _ _ msg = do
        time ← ("\n<" <>) . (<> "> ") . BS8.pack . show <$> getCurrentTime
        BS8.appendFile "db.log" $
            time <> fromLogStr (toLogStr msg)

layoutApi :: Proxy (
    "booth" :> (
             "service" :> ToServantApi BService.Route
        :<|> "service" :> ToServantApi BServiceProtected.Route
        :<|> "session" :> ToServantApi BSession.Route
        :<|> "payment" :> ToServantApi BPayment.Route
                   ))
layoutApi = Proxy

data Admin = Admin
    { adminEmail ∷ String
    , adminPassword ∷ String
    } deriving (Generic, Show)
JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 5 }) ''Admin

data ServerClient = ServerClient
    { connStr ∷ String
    , stripeTestKey ∷ String
    , stripeLiveKey ∷ String
    , stripeTestHook ∷ String
    , stripeLiveHook ∷ String
    , admin ∷ Admin
    } deriving (Generic, Show)
      deriving (ToJSON, FromJSON)

data ServerData = ServerData
    { serverBooth ∷ ServerClient
    , serverDogwalking ∷ ServerClient
    , serverServer ∷ String
    , serverEnvironment ∷ Environment
    } deriving (Generic, Show)

JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 6 }) ''ServerData

data MyException
    = MyException String
    | CantParseDataJSON
    | NoRatesFound
    deriving (Show)
    deriving anyclass (Exception)

-- | Initialize Config
initConfig ∷ IO Config
initConfig = do
    serverData ← lookupEnv "SERVERDATA" >>= \case
        Just file → readFile file
        Nothing → readFile "data.json"
    let server = fromMaybe (throw $ MyException "Can't parse 'data.json'") (decode $ BSL8.pack serverData :: Maybe ServerData)
        env' = serverEnvironment server
        boothServer = serverBooth server
        dogwalkingServer = serverDogwalking server
        sk = BS8.pack $ serverServer server
    putStrLn $ "serving port: " <> show port'
    manager ← newManager $ tlsManagerSettings

    dogwalkingPool ← runPool (BS8.pack $ connStr $ serverDogwalking server) Dogwalking.Db.migrateAll
    let dogwalkingClient = Client @'Dogwalking dogwalkingPool
            (BS8.pack $ stripeTestKey $ serverDogwalking server)
            (BS8.pack $ stripeLiveKey $ serverDogwalking server)
            (BS8.pack $ stripeTestHook $ serverDogwalking server)
            (BS8.pack $ stripeLiveHook $ serverDogwalking server)
            (adminEmail $ admin dogwalkingServer)
            (adminPassword $ admin dogwalkingServer)

    boothPool ← runPool (BS8.pack $ connStr $ serverBooth server) Booth.Db.migrateAll
    let boothClient =  Client @'Booth boothPool
            (BS8.pack $ stripeTestKey boothServer)
            (BS8.pack $ stripeLiveKey boothServer)
            (BS8.pack $ stripeTestHook boothServer)
            (BS8.pack $ stripeLiveHook boothServer)
            (adminEmail $ admin boothServer)
            (adminPassword $ admin boothServer)

    pure $ Config port' env' sk dogwalkingClient boothClient manager
  where
    port' = 8000

runPool ∷ BS8.ByteString → Migration → IO ConnectionPool
runPool str migrateAll = do
    pool' ← createPostgresqlPool str 1
    runSqlPool (runMigrationUnsafe migrateAll) pool'
    pure pool'

-- | Shutdown Server
shutdown ∷ Config → IO ()
shutdown cfg = do
    -- traverse_ dropDb ["\"user\"", "service", "payment_method"]
    traverse_ destroyAllResources [(cfg^.booth.pool)]
  -- where
    --   dropDb table = runSqlPool (rawExecute ("DROP TABLE " <> table <> " CASCADE") []) (cfg^.booth.pool)

getRates ∷ Config → IO ()
getRates cfg = do
    -- runSqlPool (rawExecute ("DROP TABLE rates CASCADE") []) (cfg^.booth.pool)
    (runSqlPool
        (P.selectFirst [] [])
        (cfg^.booth.pool)) >>= \case
        Just (_ ∷ E.Entity Booth.Db.Rates) → pure ()
        Nothing →  do
            runSqlPool (P.insert $
                       Booth.Db.Rates
                        (Rate 1000 1000)
                        (Rate 1000 1000)
                        (Rate 1000 1000)
                        (Rate 1000 1000)
                        (Rate 1000 0))
                        (cfg^.booth.pool)
            pure ()
    pure ()

addAdmin ∷ Config → IO ()
addAdmin cfg = do
    let admin = cfg^.booth.Core.Config.adminEmail
        pwd = cfg^.booth.Core.Config.adminPassword
    hashed ← liftIO $ BS8.unpack <$> hashPassword 10 (BS8.pack pwd)

    (runSqlPool
        (P.selectFirst [Booth.Db.UserEmail P.==. admin] [])
        (cfg^.booth.pool)) >>= \case
        Just _ → pure ()
        Nothing → do
            runSqlPool (P.insert $ Booth.Db.User "Admin" admin hashed "" 0) (cfg^.booth.pool)
            pure ()
    pure ()

-- | Run the app
runApp ∷ Config → IO ()
runApp cfg = do
    let app' =  Api.app cfg
    getRates cfg
    addAdmin cfg
    case cfg^.env of
       Development → runSettings (initSettings cfg) app'
       Remote → runSettings (initSettings cfg) app'
       Production → runSettings (initSettings cfg) app'
    pure ()

-- | Set default server settings such as port
initSettings ∷ Config → Settings
initSettings config =
        setHost "*6"
      $ setPort (config^.port)
      $ defaultSettings
