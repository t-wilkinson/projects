{-# LANGUAGE
  FlexibleInstances
#-}

module Core.App where

import Quaalude
import Core.Config ( HasConfig(..), Config )

import Servant ( Handler )
import Servant.Server.Generic ( AsServerT )
import Database.Selda ( MonadSelda )

newtype AppT r m a = AppT { runAppT :: ReaderT r m a}
  deriving (Functor, Applicative, Monad, MonadReader r,  MonadIO)

type AppS = AsServerT (AppT Config Handler)

unAppT :: AppT r m a -> r -> m a
unAppT = runReaderT . runAppT

appT :: (r -> m a) -> AppT r m a
appT = AppT . ReaderT

-- instance MonadSelda (AppT IO) where
--     withConnection =

-- Core.Classes?
class MonadTime m where
    getCurrentTime :: m UTCTime

-- instance MonadTime ((->) UTCTime) where
--     getCurrentTime = id

class MonadIO m => MonadLog m where
    log :: Show a => a -> m ()
    log = liftIO . print . show

instance MonadIO m => MonadLog (AppT r m)
