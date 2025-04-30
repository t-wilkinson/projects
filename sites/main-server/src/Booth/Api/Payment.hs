{-# Language TemplateHaskell #-}

module Booth.Api.Payment where

import Quaalude
import Core.App (AppS)
import Core.Config (Config, sk)
import Core.Helpers (stripeReqBS, parseStripeRes)
import Booth.Db
import Booth.Service (AddService(..), ServiceCharge(..), addService, serviceCharge, serviceLength)
import Booth.Helpers (getStripeKey)
import Booth.Db.Types
import Core.Token (SToken, suntokenize, tokenize)
import qualified Core.Errors as CE

import qualified Data.Map as Map
import qualified Data.Aeson.Lens as JSON
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Char8 as BS8

data ReturnFeePaymentIntent = ReturnFeePaymentIntent
    { clientSecret ∷ String
    , charge ∷ Int
    , token ∷ SToken
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data ReturnPaymentIntent = ReturnPaymentIntent
    { clientSecret ∷ String
    , charge ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data ConfirmPaymentMethod = ConfirmPaymentMethod
    { paymentIntentId ∷ String
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data Route r = Route
    { _createPaymentIntent ∷ r
        :- "intent"
        :> Description "Create a new payment intent."
        :> Post '[JSON] ReturnPaymentIntent
    , _createFeePaymentIntent ∷ r
        :- "fee"
        :> "intent"
        :> Description "Create a new payment intent."
        :> ReqBody '[JSON] AddService
        :> Post '[JSON] ReturnFeePaymentIntent
    , _createFeePaymentIntentToken ∷ r
        :- "fee"
        :> "intent"
        :> Capture "token" SToken
        :> ReqBody '[JSON] AddService
        :> Post '[JSON] ReturnFeePaymentIntent
    , _getCharge ∷ r
        :- "charge"
        :> Description "Get how much money user has in their account."
        :> Get '[JSON] Double
    } deriving (Generic)

route ∷ E.Entity User → Route AppS
route user = Route
    (createPaymentIntent user)
    (createFeePaymentIntent user)
    (createFeePaymentIntentToken user)
    (getCharge user)

createPaymentIntent
    ∷  ( MonadIO m
       , MonadReader Config m
       , MonadError ServerError m
       )
    ⇒ E.Entity User
    → m ReturnPaymentIntent
createPaymentIntent entityUser@E.Entity{..} = do
    -- Get payment intents from stripe
    charge ← ceiling <$> getUserCharge entityUser
    key ← getStripeKey
    let req =
            [ ("amount", show charge)
            , ("currency", "usd")
            , ("customer", userCustomerId entityVal)
            ]
    res ← stripeReqBS key req POST ["v1","payment_intents"]
    clientSecret ← parseStripeRes $ res ^? JSON.key "client_secret"

    pure $ ReturnPaymentIntent clientSecret charge

getFeePaymentIntentCharge
    ∷  ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → AddService
    → m Int
getFeePaymentIntentCharge E.Entity{..} service@AddService{..} = do
    Rate{..} ← (runDb $ P.selectFirst [] []) >>= \case
        Just (E.Entity _ val) → pure $ ratesService val
        Nothing → throwError CE.noRates
    curTime ← liftIO $ getCurrentTime
    let Date{..} = date
        isServiceInFuture = (curTime >= UTCTime
                (fromGregorian (toInteger dateYear) dateMonth dateDay)
                -- 300 (5min) because frontend uses blocks of 5min
                (secondsToDiffTime $ 300 + toInteger start))
    when isServiceInFuture (throwError CE.serviceInPast)
    let dt = serviceLength start end
        charge = ceiling $ (fromIntegral rateTrack + (fromIntegral (rateDuration * dt)) / 60 ∷ Double)
    pure charge

feePaymentIntent
    ∷  ( MonadIO m
       , MonadReader Config m
       , MonadError ServerError m
       )
    ⇒ Int
    → Key Service
    → SToken
    → User
    → m ReturnFeePaymentIntent
feePaymentIntent charge serviceId token user = do
    -- Get payment intents from stripe
    key ← getStripeKey
    let req =
            [ ("amount", show charge)
            , ("currency", "usd")
            , ("customer", userCustomerId user)
            , ("metadata[serviceId]", BSL8.unpack $ encode serviceId)
            , ("metadata[paymentType]", BSL8.unpack $ encode ServiceFee)
            ]
    res ← stripeReqBS key req POST ["v1","payment_intents"]
    clientSecret ← parseStripeRes $ res ^? JSON.key "client_secret"
    pure $ ReturnFeePaymentIntent clientSecret charge token

-- Create payment intent for the service fee
-- Also add the service
createFeePaymentIntent
    ∷  ( MonadIO m
       , MonadReader Config m
       , MonadError ServerError m
       )
    ⇒ E.Entity User
    → AddService
    → m ReturnFeePaymentIntent
createFeePaymentIntent entityUser@E.Entity{..} service@AddService{..} = do
    charge ← getFeePaymentIntentCharge entityUser service
    sk' ← asks(^.sk)
    serviceId ← addService entityUser service
    let token = BS8.unpack . tokenize sk' . BSL8.toStrict . encode $ serviceId
    feePaymentIntent charge serviceId token entityVal

createFeePaymentIntentToken
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → SToken
    → AddService
    → m ReturnFeePaymentIntent
createFeePaymentIntentToken entityUser@E.Entity{..} token service@AddService{..} = do
    charge ← getFeePaymentIntentCharge entityUser service
    sk' ← asks(^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    serviceId ∷ Key Service ← case maybeKey of
        Nothing → throwError CE.token
        Just key → pure key
    feePaymentIntent charge serviceId token entityVal

getCharge
    ∷  ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → m Double
getCharge entityUser = do
    getUserCharge entityUser

getUserCharge
    ∷   ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → m Double
getUserCharge E.Entity{entityKey, entityVal} = do
    let totalPayed = fromIntegral $ userTotalPayed entityVal

    -- Get all services owned by user
    services ∷ [E.Entity Service] ← runDb $ E.select $ E.from \s → do
        E.where_ $ s E.^. ServiceUId E.==. E.val entityKey
        pure s
    curTime ← liftIO getCurrentTime

    mayRates ∷ Maybe (P.Entity Rates) ← runDb $ P.selectFirst [] []
    rates ← maybe (throwError CE.noRates) (pure . E.entityVal) mayRates

    serviceCharges ← traverse (toServiceCharge . E.entityVal) services
    let charge = foldr (serviceCharge (Just curTime) rates) (-totalPayed) serviceCharges
    pure $ max 0 charge

toServiceCharge ∷ (MonadIO m, MonadReader Config m) ⇒ Service → m ServiceCharge
toServiceCharge Service{..} = do
    promos ← runDb $ Map.elems <$> P.getMany servicePromos
    pure $ ServiceCharge serviceStatus (Just serviceDate) serviceStart serviceEnd serviceTracks serviceMixing serviceMastering serviceRecording promos
