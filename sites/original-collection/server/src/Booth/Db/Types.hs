{-# Language TemplateHaskell #-}

module Booth.Db.Types where

import Quaalude
import Database.Persist.TH
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON

data Status
    = NotConfirmed -- Just as a reference for future manipulations
    | Processing    -- Stripe is just about to confirm purchase
    | PaymentProcessing    -- First webhook recieved. Service not considered scheduled.
    | Failed        -- Payment failed. Service not considered scheduled.
    | Created        -- Payment success. Will be charged as normal
    | Removed        -- Admin. Service will not be charged and not considered scheduled
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PaymentType
    = ServiceFee
    | AccountCharge
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Date = Date
    { dateYear :: Int
    , dateMonth :: Int
    , dateDay :: Int    -- Day of month
    } deriving (Show, Read, Eq, Generic)

JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 4 }) ''Date

data PromoType
    = PromoShareCode String  -- Share code of other user
    | PromoDiscount -- Discount
    | PromoPercent -- Percentage
    | PromoStatic -- Static discount
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Rate = Rate
    { rateTrack ∷ Int  -- Rate per track
    , rateDuration ∷ Int  -- Rate per session duration
    } deriving (Show, Read, Eq, Generic)
JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 4 }) ''Rate

data Expires
    = ExpiresDate Date
    | ExpiresAmount Int -- cannot add promo after `Int` promos
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

derivePersistField "Status"
derivePersistField "Date"
derivePersistField "PromoType"
derivePersistField "Rate"
derivePersistField "Expires"
