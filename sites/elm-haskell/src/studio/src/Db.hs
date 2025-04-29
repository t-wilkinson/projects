-- | Generate data types for database

module Db
  ( toVal
  , runDb
  ) where

import Config ( App, pool)
  
import Database.Persist.Sql ( SqlPersistT, runSqlPool )
import Database.Persist.Types ( Entity(entityVal) )


-- | grab entity value from query
toVal :: Functor f => App (f (Entity a)) -> App (f a)
toVal = fmap (fmap entityVal)

-- | Asking for pool from 'Config' then running the query
runDb :: SqlPersistT IO a -> App a
runDb query = do
  pool <- asks (^. pool)
  liftIO $ runSqlPool query pool
