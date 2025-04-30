module Dogwalking.Service where

import Quaalude
import Dogwalking.Db.Status

data Rates = Rates
    { rRate ∷ Int
    } deriving (Show, Eq)

-- cents per minute
rates ∷ Rates
rates = Rates
    { rRate = 40
    }

data ServiceCharge = ServiceCharge
    { status ∷ Status
    , year ∷ Int, month ∷ Int, day ∷ Int
    , start ∷ Int, end ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

{-# INLINE serviceCharge #-}
serviceCharge ∷ UTCTime → ServiceCharge → Int
serviceCharge curTime ServiceCharge{..} =
    500 + case
        ( curTime >= (UTCTime    -- was service in the past?
            (fromGregorian (toInteger year) month day)
            (secondsToDiffTime (toInteger end)))
        , end - start > 0       -- Also checked elsewhere but make 100% this is the case
        , status                -- 'Status' must be 'Normal'
        ) of
        (True, True, Normal) →
            let dt = (end - start) `div` 60
            in foldr (\x a → (a +) $ (dt * x)) 0 [rRate rates]
        _ → 0

