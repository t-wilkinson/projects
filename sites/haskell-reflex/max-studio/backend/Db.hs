module Db where

import Quaalude
import Core.App (AppT)
import Core.Config (Config, pool)

import Database.Persist.Sql (SqlPersistT, runSqlPool)

-- | Asking for pool from 'Config' then running the query
runDb
    :: (MonadIO m, MonadReader Config m)
    => SqlPersistT IO a -> m a
runDb query = do
    pool <- asks (^. pool)
    liftIO $ runSqlPool query pool
