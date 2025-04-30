{-# Language
  TemplateHaskell
#-}

module Core.Config where

import Quaalude
import Control.Lens ( makeClassy )
import Data.Pool ( Pool )
import Database.Selda.Backend ( SeldaConnection )
import Database.Selda.SQLite ( SQLite )

-- | Transition between environments
data Environment
  = Production
  | Development
  | Test
  deriving (Read)

-- | The Config for the application containing the environment.
data Config = Config
  { _port :: Int  -- ^ Used to shutdown the database
  , _env :: Environment  -- ^ Distinguish environments
  , _pool :: Pool (SeldaConnection SQLite)  -- ^ Used to process postgresql functions
  }

makeClassy ''Config

