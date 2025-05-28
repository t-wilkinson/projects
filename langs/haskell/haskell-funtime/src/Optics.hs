{-# LANGUAGE GeneralizedNewtypeDeriving,LambdaCase #-}

module Optics where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Except


class HasNetConfig t where
  netConfig :: Lens' t NetConfig
  netPort :: Lens' t NetPort
  netPort = netConfig . netPort

class HasDatabaseConfig t where
  dbConfig :: Lens' t DbConfig
  dbConn :: Lens' t DbConnection
  dbPool :: Lens' t DbPool
  dbConn   = dbConfig . dbConn
  dbPool   = dbConfig . dbPool

class AsDbError t where
  _DbError :: Prism' t DbError
  _DbErr1 :: Prism' t String
  _DbErr2 :: Prism' t ()
  _DbErr1 = _DbError . _DbErr1
  _DbErr2 = _DbError . _DbErr2


class AsNetError t where
  _NetError :: Prism' t NetError
  _NetErr1 :: Prism' t Int
  _NetErr1 = _NetError . _NetErr1


-- | App
data AppConfig =
  AppConfig { _appDbConfig :: DbConfig
            , _appNetConfig :: NetConfig
            } deriving (Show)

instance HasDatabaseConfig AppConfig where
  dbConfig = lens _appDbConfig (\d c -> d { _appDbConfig = c })

instance HasNetConfig AppConfig where
  netConfig = lens _appNetConfig (\d c -> d { _appNetConfig = c })
  netPort   = netConfig . netPort


-- | DbConfig
data DbConfig =
  DbConfig { _dbPool :: DbPool
           , _dbConn :: DbConnection
           } deriving (Show)
type DbPool = String
type DbConnection = String

instance HasDatabaseConfig DbConfig where
  dbConfig = id
  dbConn   = lens _dbConn (\d c -> d { _dbConn = c })
  dbPool   = lens _dbPool (\d s -> d { _dbPool = s })


-- | NetConfig
type NetPort = Int
data NetConfig =
  NetConfig { _netPort :: NetPort
            } deriving (Show)

instance HasNetConfig NetConfig where
  netConfig = id
  netPort   = lens _netPort (\d c -> d { _netPort = c })


-- | Error
data AppError
  = AppDbError { _dbError :: DbError }
  | AppNetError { _netError :: NetError}


instance AsNetError AppError where
  _NetError = prism AppNetError $ \case
    AppNetError ne -> Right ne
    _              -> Left (AppNetError (NetErr1 404))

instance AsDbError AppError where
  _DbError = prism AppDbError $ \case
    AppDbError ne -> Right ne
    _             -> Left (AppDbError (DbErr1 "erorororor"))

-- | DbError
data DbError
  = DbErr1 String
  | DbErr2 ()

instance AsDbError DbError where
  _DbError = id
  _DbErr1  = prism DbErr1 $ \case
    _ -> Left (DbErr1 "error")
  _DbErr2 = prism DbErr2 $ \case
    _ -> Left (DbErr2 ())

-- | NetError
data NetError
  = NetErr1 Int

instance AsNetError NetError where
  _NetError = id
  _NetErr1  = prism NetErr1 $ \case
    NetErr1 t -> Right t


-- | Body
newtype AppT m a =
  AppT { runApp :: ReaderT AppConfig (ExceptT AppError m) a
       } deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader AppConfig,  MonadIO)


testRun :: AppT IO ()
testRun = do
  conf <- local (set dbConn "new connection") ask
  liftIO $ print conf
  return ()

testOptics :: IO ()
testOptics = do
  let config = AppConfig
        { _appDbConfig  = DbConfig { _dbPool = "database pool"
                                   , _dbConn = "localhost"
                                   }
        , _appNetConfig = NetConfig { _netPort = 8080 }
        }
  runExceptT $ runReaderT (runApp testRun) config
  return ()
