{-# Language FunctionalDependencies #-}

module Core.Client where

import Quaalude
import Database.Persist.Sql (SqlPersistT)
import Core.Config (Config)

data Client
    = Dogwalking
    | Booth

class (MonadIO m, MonadReader Config m) ⇒ HasDb (client ∷ Client) (m ∷ * → *)
  | m → client where
    runDb ∷ SqlPersistT IO a → m a
    migrate ∷ m ()
    runPool ∷ m ()
