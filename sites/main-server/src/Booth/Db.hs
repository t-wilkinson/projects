{-# Language
  InstanceSigs
, DeriveDataTypeable
, QuasiQuotes
, StandaloneDeriving
, TemplateHaskell
, UndecidableInstances
, GADTs
#-}

module Booth.Db where

import Quaalude

import Language.Haskell.TH
import Database.Persist.TH
import Database.Persist.Postgresql
import Booth.Db.Types
import Core.Config (Config, booth, pool)

runDb ∷ (MonadIO m, MonadReader Config m) ⇒ SqlPersistT IO a → m a
runDb query = do
    pool' ← asks (^.booth.pool)
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
    name String
    email String
    UniqueEmail email
    password String
    customerId String -- customer id created when registering
    totalPayed Word default=0 -- in smallest units of currency (cents)
    deriving Eq Show

PaymentMethod
    paymentId String
    uId UserId

Service json
    uId UserId
    description String
    status Status

    date Date
    start Int   -- minutes from start of service
    end Int     -- minutes from end of service

    tracks Int
    mixing Bool
    mastering Bool
    recording Bool

    promos [PromoId]
    deriving Show Eq

Promo json
    code String
    UniqueCode code
    expires Expires            -- can't add promo after it expires
    promoType PromoType
    amount Int
    deriving Show Eq

Rates json      -- Only one
    base Rate
    mixing Rate
    mastering Rate
    recording Rate
    service Rate

    deriving Show Eq

Project json
    group String
    alt String
    href String
    src String
    order Int
    deriving Show Eq

|]
