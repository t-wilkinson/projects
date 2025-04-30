{-# LANGUAGE QuasiQuotes #-}

-- | Functions for SQL queries

module Db.Functions where

import qualified Database.PostgreSQL.Simple as Sql


-- | Performs a query without arguments and returns the resulting rows.
queryRaw q = withPool $ \conn -> Sql.query_ conn q


-- | Perform action that needs database connection.
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f
