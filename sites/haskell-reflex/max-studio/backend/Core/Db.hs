{-# Language
  InstanceSigs
, DeriveDataTypeable
, DerivingStrategies
, FlexibleContexts
, FlexibleInstances
, GADTs
, GeneralizedNewtypeDeriving
, MultiParamTypeClasses
, QuasiQuotes
, StandaloneDeriving
, TemplateHaskell
, TypeFamilies
, UndecidableInstances
#-}

module Core.Db where

import Quaalude

import Language.Haskell.TH
import Database.Persist.TH
import Database.Persist.Postgresql


share [mkPersist
    ((mkPersistSettings (ConT ''SqlBackend))
        { mpsGeneric = True
        , mpsPrefixFields = True
        , mpsEntityJSON =
          Just EntityJSON { entityToJSON = 'entityIdToJSON
                          , entityFromJSON = 'entityIdFromJSON
                          }
        , mpsGenerateLenses = False
        -- , mpsDeriveInstances = [''Show, ''Eq]
        }) , mkMigrate "migrateAll"] [persistLowerCase|
RentRequest json
    start UTCTime
    end UTCTime
    date Day
    deriving Eq Show

User json
    name String
    password String
    email String
    phone_number String Maybe
    requests [RentRequest]
    UniqueEmail email
    deriving Eq Show
|]

