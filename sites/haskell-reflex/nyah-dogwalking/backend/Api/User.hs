-- {-# LANGUAGE
-- #-}

module Api.User where

import Quaalude
import Common ( User(..) )
import Core.App ( AppS, MonadLog(log) )
import Core.Config ( Config, HasConfig(..) )
import Db ( runDb )

import Servant ( err404 )

data Route r = Route
    { _login :: r
        :- "login"
        :> ReqBody '[JSON] User
        :> Post '[JSON] ()
    } deriving Generic

route = Route @AppS login

login ::
    ( HasConfig r
    , MonadReader r m
    , MonadIO m
    , MonadLog m
    )
  => User
  -> m ()
login user = do
    log user
    -- pure name
    pool <- asks (^.pool)
    log "logging"
    -- runDb $ print "test"
    pure ()
    -- throw err404 { errBody = "Something hello etherfind User.hsw " }

