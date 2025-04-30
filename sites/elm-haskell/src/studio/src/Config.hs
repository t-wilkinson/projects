{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | AppT and Config types

module Config where

import Control.Monad.Logger ( MonadLogger, monadLoggerLog, fromLogStr, toLogStr)
import Database.Persist.Postgresql ( ConnectionPool )
import Control.Monad.Except ( MonadError(..) )
import Control.Lens ( makeClassy )
import Servant (Handler)
import Language.Haskell.TH hiding (AppT)
import Database.Persist.TH  hiding (AppT)
import Database.Persist.Sql
import Control.Exception ( throwIO, catch, try )


-- | Transition between environments
data Environment
  = Production
  | Development
  | Test
  deriving (Read)

data AppError = AppError deriving ( Show )
newtype AppException = AppException
  { unAppException :: AppError
  } deriving ( Show )
instance Exception AppException

-- | Wrapper for servant monad
newtype AppT m a = AppT { runAppT :: ReaderT Config m a}
  deriving (Functor, Applicative, Monad, MonadReader Config,  MonadIO)
type App = AppT Handler
type AppS = AsServerT App

-- | Disgusting solution for simple @createPostGresqlPool@ function
instance MonadLogger IO  where
  monadLoggerLog _ _ _ = print . fromLogStr . toLogStr

-- instance MonadError AppError App where
--   throwError :: AppError -> App a
--   throwError = liftIO . throwIO . AppException

--   catchError :: App a -> (AppError -> App a) -> App a
--   catchError action handler = do
--     let ioAction = runHandler' . runReaderT . runAppT $ action
--     ioAction `catch`


-- instance MonadFail App where
--   fail _ = AppT

-- instance MonadIO m => MonadLogger (AppT m) where
--   monadLoggerLog loc src lvl msg = lift $ monadLoggerLog loc src lvl msg

-- | The Config for the application containing the environment.
data Config = Config
  { _port :: Int  -- ^ Used to shutdown the database
  , _pool :: ConnectionPool  -- ^ Used to process postgresql functions
  , _env :: Environment  -- ^ Distinguish environments
  }

makeClassy ''Config


settings :: MkPersistSettings
settings =
  (mkPersistSettings (ConT ''SqlBackend))
      { mpsGeneric = False
      , mpsPrefixFields = False
      , mpsEntityJSON =
        Just EntityJSON { entityToJSON = 'entityIdToJSON
                        , entityFromJSON = 'entityIdFromJSON
                        }
      , mpsGenerateLenses = False
      -- , mpsDeriveInstances = [Show, Eq]
      }
share' = share
    [mkPersist ((mkPersistSettings $ ConT ''SqlBackend) {mpsPrefixFields = False}), mkMigrate "migrateAll"]
