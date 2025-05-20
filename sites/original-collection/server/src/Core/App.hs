module Core.App where

import Quaalude
import Core.Config (Config)

import Control.Monad.Trans.Except (runExceptT)
import Crypto.KDF.BCrypt  (hashPassword)
import Data.ByteString.Char8 (pack, unpack)

type role AppT representational representational nominal
newtype AppT r m a = AppT
    { runAppT ∷ ReaderT r (ExceptT ServerError m) a
    } deriving newtype ( Functor, Applicative, Monad
                       , MonadIO, MonadThrow, MonadFix, MonadReader r, MonadError ServerError
                       )

type AppS = AsServerT (AppT Config IO)
type App = AppT Config IO

unAppT ∷ ∀ m r a. AppT r m a → r → m (Either ServerError a)
unAppT app r = runExceptT $ flip runReaderT r $ runAppT app

appT ∷ (Monad m) ⇒ (r → m a) → AppT r m a
appT f = AppT . ReaderT $ \r → ExceptT $ Right <$> f r

class MonadTime m where
    getCurrentTime ∷ m UTCTime
instance MonadTime ((→) UTCTime) where
    getCurrentTime = identity

-- log time/data (conn info) etc
class (MonadIO m) ⇒ MonadLog m where
    log ∷ Show a ⇒ a → m ()
instance MonadIO m ⇒ MonadLog (AppT r m) where
    log msg = liftIO do
        print msg
        appendFile "../db.log" $ show msg

class (MonadIO m) ⇒ HashPassword m where
    hash ∷ String → m Hash
instance (MonadIO m) ⇒ HashPassword (AppT r m) where
    hash password = liftIO $ unpack <$> hashPassword 10 (pack password)

type Hash = String
