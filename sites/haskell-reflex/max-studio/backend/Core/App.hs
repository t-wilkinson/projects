module Core.App where

import Quaalude
import Core.Config (HasConfig(..), Config)

import Servant (Handler)
import Control.Monad.Catch (MonadThrow)

import Crypto.KDF.BCrypt  (hashPassword, validatePassword, bcrypt)
import Crypto.Random.Types (MonadRandom(..))
import Data.ByteString.Char8 (pack, unpack)
import System.IO (IOMode(..), withFile, hPrint, hPutStrLn)

newtype AppT r m a = AppT
    { runAppT :: ReaderT r m a
    } deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadIO, MonadThrow)

type AppS = AsServerT (AppT Config Handler)

unAppT :: AppT r m a -> r -> m a
unAppT = runReaderT . runAppT

appT :: (r -> m a) -> AppT r m a
appT = AppT . ReaderT

-- Core.Classes?
class MonadTime m where
    getCurrentTime :: m UTCTime
instance MonadTime ((->) UTCTime) where
    getCurrentTime = identity

-- log time/data (conn info) etc
-- maybe this should be instead 'MonadLogger'
class (MonadIO m) => MonadLog m where
    log :: Show a => a -> m ()
instance MonadIO m => MonadLog (AppT r m) where
    log msg = do
        liftIO $ print msg
        liftIO $ withFile "../db.log" AppendMode (\h -> hPutStrLn h $ show msg)

class (MonadIO m) => HashPassword m where
    hash :: String -> m String
instance (MonadIO m) => HashPassword (AppT r m) where
    hash password = liftIO $ unpack <$> hashPassword 10 (pack password)

