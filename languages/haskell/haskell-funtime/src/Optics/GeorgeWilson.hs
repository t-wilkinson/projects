module Optics.GeorgeWilson where

-- | 
{-
import Control.Lens
  

data DbConfig = DbConfig
  { _dbConn :: DbConnection
  , _dbSchema :: DbSchema
  }

class HasDbConfig t where
  dbConfig :: Lens t DbConfig
  dbConn :: Lens t DbConnection
  dbSchema :: Lens t Schema
  dbConn = dbConfig . dbConn
  dbSchema = dbConfig . dbSchema

instance HasDbConfig DbConfig where
  dbConfig = id
  dbConn = lens _dbConn (\d c -> d { _dbConn = c })
  dbSchema = lens _dbSchema (\d s -> d { _dbSchema = s})

-- Our app config can now be an instance of these classes and provide access to its contents
data AppConfig = AppConfig
  { appDbConfig :: DbConfig
  , appNetConfig :: NetworkConfig
  }

instance HasDbConfig AppConfig where
  dbConfig = lens appDbConfig (\app db -> app { appDbConfig = db })


-- A prism is like a first-class pattern match
-- It lets us get at one branch of an ADT
preview :: Prism a b -> (a -> Maybe b) -- partial getter
review :: Prism a b -> (b -> a)  -- Constructor
prism ::  -- Construction
  (target -> source)
  -> (source -> Maybe target)
  -> Prism source target

-- >>> preview _Left (Left (Just 4))
-- (Just (Just 4))
-- >>> preview (_Left . _Just) (Left (Just 4))
-- Just 4
-- >>> review (_Right . _Just) "hello"
-- Right (Just "hello")
-- >>> review (_Just . _Left) 42
-- Just (Left 42)

data DbError
  = QueryError Text
  | InvalidConnection

class AsDbError t where
  _DbError :: Prism t DbError
  _QueryError :: Prism t Text
  _InvalidConn :: Prism t ()
  _QueryError = _DbError . _QueryError
  _InvalidConn = _DbError . _InvalidConn

instance AsDbError DbError where
  _DbError = id
  _QueryError = prism QueryError $
    \case QueryError t -> Just t
          _ -> Nothing
  _InvalidConn = prism InvalidConnection $
    \case InvalidConnection -> Just ()
          _ -> Nothing

data AppError
  = AppDbError { dbError :: DbError }
  | AppNetError { netError :: NetworkError }

instance AsDbError AppError where
  _DbError = prism AppDbError $
    \case AppDbError dbe -> Just dbe
          _ -> Nothing

-- instance AsNetworkError AppError where
--   _NetworkError = prism AppNetError $
--     \case AppNetError ne -> Just ne
--           _ -> Nothing

newtype App a = App
  { unApp :: ReaderT AppConfig (ExceptT AppError IO) a
  } deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadError AppError, MonadIO)

-- or
-- makeClassyPrisms ''NetworkError
-- makeClassy ''DbConfig

-- we can now::
loadFromDb ::
  ( MonadError e m, MonadReader r m
  , AsDbError e, HasDbConfig r
  , MonadIO m
  ) => m MyData

sendOverNet ::
  ( MonadError e m, MonadReader r m
  , AsNetworkError e, HasNetworkConfig r
  , MonadIO m
  ) => MyData -> m ()

loadAndSend ::
  ( MonadError e m, MonadReader r m
  , AsNetworkError e, HasDbConfig r
  , AsDbError e, HasNetworkConfig r
  , MonadIO m
  ) => m ()
  loadAndSend = loadFromDb >>= sendOverNet

-}
