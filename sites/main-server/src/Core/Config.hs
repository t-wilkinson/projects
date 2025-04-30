{-# Language
  TemplateHaskell
, FunctionalDependencies
#-}

module Core.Config where

import Quaalude
import Control.Lens (makeClassy)
import Database.Persist.Postgresql (ConnectionPool)
import Network.HTTP.Client (Manager)
import Data.ByteString.Char8 (ByteString)

-- | Transition between environments
data Environment
  = Development -- Normally here
  | Remote -- Just barely not production. First real test of server.
  | Production -- Production ready, charge accounts, etc.
  deriving (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Database
    = PostgreSQL
    | Sqlite
    | MongoDB
    | Redis
    deriving (Show)

type ServerKey = ByteString
type PrivateID = ByteString

data ClientName
    = Dogwalking
    | Booth

-- | 'Client' paramaterized by 'ClientName'
data Client (a ∷ ClientName) = Client
    { _pool ∷ ConnectionPool
    , _stripeTestKey ∷ ServerKey
    , _stripeLiveKey ∷ ServerKey
    , _stripeTestHook ∷ ServerKey
    , _stripeLiveHook ∷ ServerKey
    , _adminEmail ∷ String
    , _adminPassword ∷ String
    } deriving (Show)

-- | The Config for the application containing the environment.
data Config = Config
    { _port       ∷ Int  -- ^ Used to shutdown the database
    , _env        ∷ Environment  -- ^ Distinguish environments
    , _sk         ∷ ServerKey  -- ^ Private server key for encryption
    , _dogwalking ∷ Client 'Dogwalking
    , _booth      ∷ Client 'Booth
    , _manager    ∷ Manager    -- ^ http connection manager
    }

makeClassy ''Client
makeClassy ''Config
