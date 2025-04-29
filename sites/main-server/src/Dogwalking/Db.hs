{-# Language
  InstanceSigs
, DeriveDataTypeable
, QuasiQuotes
, StandaloneDeriving
, TemplateHaskell
, UndecidableInstances
, GADTs
#-}

module Dogwalking.Db where

import Quaalude
import Dogwalking.Db.Status

import Language.Haskell.TH
import Database.Persist.TH
import Database.Persist.Postgresql
import Core.Config (Config, dogwalking, pool)

runDb ∷ (MonadIO m, MonadReader Config m) ⇒ SqlPersistT IO a → m a
runDb query = do
    pool' ← asks (^.dogwalking.pool)
    liftIO $ runSqlPool query pool'

share [mkPersist
    ((mkPersistSettings (ConT ''SqlBackend))
        { mpsBackend = ConT ''SqlBackend
        , mpsGeneric = False
        , mpsPrefixFields = True
        , mpsEntityJSON =
          Just EntityJSON { entityToJSON = 'entityIdToJSON
                          , entityFromJSON = 'entityIdFromJSON
                          }
        , mpsGenerateLenses = False
        , mpsDeriveInstances = []
        }) , mkMigrate "migrateAll"] [persistLowerCase|

User json
    email String
    UniqueEmail email
    password String
    street String
    city String
    state String
    zipcode Int
    deriving Eq Show

Service json
    uId UserId
    description String
    status Status
    year Int
    month Int
    day Int
    start Int
    end Int
    deriving Show Eq

|]



