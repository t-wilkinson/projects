{-# Language
  TemplateHaskell
#-}

module Core.Config where

import Quaalude
-- import Database.Selda.Backend (SeldaConnection) -- remove
-- import Database.Selda.SQLite (SQLite) -- remove
-- import Data.Pool (Pool)
import Control.Lens (makeClassy)
import Database.Persist.Postgresql (ConnectionPool)
import MyCookie (AuthCookieSettings, ServerKeySet, PersistentServerKey, RandomSource, SessionSettings)

-- | Transition between environments
data Environment
  = Production
  | Development
  deriving (Read)

data Database
    = PostgreSQL
    | Sqlite
    | MongoDB
    | Redis
    deriving (Read, Show)

-- | The Config for the application containing the environment.
data Config = Config
    { _port :: Int  -- ^ Used to shutdown the database
    , _env :: Environment  -- ^ Distinguish environments
    , _pool :: ConnectionPool  -- ^ Used to process postgresql functions
    , _authSettings :: AuthCookieSettings
    , _serverKeySet :: PersistentServerKey
    , _randomSource :: RandomSource
    , _sessionSettings :: SessionSettings
    }

makeClassy ''Config
