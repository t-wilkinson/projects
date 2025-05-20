{-# LANGUAGE TemplateHaskell #-}

module Booth.Api.Service where

import Booth.Db
import Booth.Db.Types (Status(..), Date(..))
import Core.App (AppS)
import Core.Config (Config, ServerKey, sk)
import Core.Token (SToken, tokenize, suntokenize)
import Quaalude
import Booth.Service (ServiceCharge(ServiceCharge), serviceCharge, getOverlappingServices)
import Booth.Helpers (isAdmin)
import qualified Core.Errors as CE

import qualified Data.Map as Map
import qualified Data.Time           as  Time
import qualified Database.Esqueleto  as  E
import qualified Database.Persist    as  P
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON

data ReturnService = ReturnService
    { token ∷ SToken
    , description ∷ String
    , owned ∷ Bool
    , status ∷ Status
    , date ∷ Date
    , start ∷ Int , end ∷ Int
    , tracks ∷ Int, mixing ∷ Bool, mastering ∷ Bool, recording ∷ Bool
    , promos ∷ [Promo]
    } deriving (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromJSON)

data MaybeGetService = MaybeGetService
    { maybeYear ∷ Maybe Integer
    , maybeMonth ∷ Maybe Int
    , maybeDay ∷ Maybe Int
    } deriving (Generic)

-- maybe<Name> → <name>
JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 5 }) ''MaybeGetService

data GetServiceCharge = GetServiceCharge
    { token ∷ Maybe SToken
    , start ∷ Int, end ∷ Int
    , day ∷ Int, month ∷ Int, year ∷ Int
    , tracks ∷ Int, mixing ∷ Maybe Bool, mastering ∷ Maybe Bool, recording ∷ Maybe Bool
    , promos ∷ [Promo]
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

-- Should use queries here
data Route r = Route
    { _getSerivceCharge ∷ r
        :- "charge"
        :> ReqBody '[JSON] GetServiceCharge
        :> Post '[JSON] Double
    , _getServices ∷ r
        :- "day"
        :> ReqBody '[JSON] MaybeGetService
        :> Post '[JSON] [ReturnService]
    , _getServicesByMonth ∷ r
        :- "month"
        :> Description "Get all services in a given month"
        :> ReqBody '[JSON] MaybeGetService
        :> Post '[JSON] [ReturnService]
    } deriving (Generic)

route ∷ (Maybe (E.Entity User), Rates) → Route AppS
route (maybeUser, rates) = Route
    (getServiceCharge rates)
    (getServices maybeUser)
    (getServicesByMonth maybeUser)

getServiceCharge
    ∷ ∀ m.
        ( MonadIO m
        , MonadError ServerError m
        , MonadReader Config m
        )
    ⇒ Rates → GetServiceCharge → m Double
getServiceCharge rates GetServiceCharge{..} = do
    sk' ← asks(^.sk)
    let mayServiceKey = do
            token' ← token
            key ∷ (Key Service) ← JSON.decode $ BSL8.pack $ suntokenize sk' token'
            pure key

    services ← getOverlappingServices mayServiceKey year month day start end
    case services of
        [] → pure $ serviceCharge Nothing rates toServiceCharge 0
        _ → throwError CE.serviceOverlapping
  where
    isTrue = fromMaybe False
    toServiceCharge = ServiceCharge
        Created Nothing start end
        tracks (isTrue mixing) (isTrue mastering) (isTrue recording)
        promos

getServices
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Maybe (E.Entity User)
    → MaybeGetService
    → m [ReturnService]
getServices maybeUser maybeGetService = do
    sk' ← asks(^.sk)
    curUtctDay ← liftIO $ utctDay <$> getCurrentTime
    let Date{..} = getServiceFromMaybe curUtctDay maybeGetService
    services ∷ [E.Entity Service] ← runDb $
        E.select $
            E.from \services → do
                E.where_ $
                    services E.^. ServiceDate E.==. E.val (Date
                        (fromIntegral dateYear) dateMonth dateDay)
                pure services
    toReturnService sk' maybeUser services

getServicesByMonth
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Maybe (E.Entity User)
    → MaybeGetService
    → m [ReturnService]
getServicesByMonth maybeUser maybeGetService = do
    sk' ← asks(^.sk)
    curUtctDay ← liftIO $ utctDay <$> getCurrentTime
    let Date{dateYear, dateMonth} = getServiceFromMaybe curUtctDay maybeGetService
    services ∷ [E.Entity Service] ← runDb $ E.select $ E.from $ \s → do
        pure s
    let services' = flip filter services \E.Entity{..} →
            let (Date sYear sMonth _) = serviceDate entityVal
            in sYear == dateYear && sMonth == dateMonth
    toReturnService sk' maybeUser services'

-- | Default to current date if request does not specify a field.
getServiceFromMaybe ∷ Time.Day → MaybeGetService → Date
getServiceFromMaybe utctDay MaybeGetService{maybeYear, maybeMonth, maybeDay} =
    let (curYear, curMonth, curDay) = Time.toGregorian utctDay
    in Date
        (fromIntegral $ fromMaybe curYear maybeYear)
        (fromMaybe curMonth maybeMonth)
        (fromMaybe curDay maybeDay)

toReturnService
    ∷ ∀ m.
        ( Monad m
        , MonadReader Config m
        , MonadIO m
        )
    ⇒ ServerKey → Maybe (E.Entity User) → [E.Entity Service] → m [ReturnService]
toReturnService sk' Nothing services = traverse (serviceToReturnService sk' (const False)) services
toReturnService sk' (Just E.Entity{..}) services = do
    isAdmin' ← isAdmin entityVal
    let isValidUser check = check == entityKey || isAdmin'
    traverse (serviceToReturnService sk' isValidUser) services

serviceToReturnService
    ∷ ∀ m.
        ( Monad m
        , MonadReader Config m
        , MonadIO m
        )
    ⇒ ServerKey → (UserId → Bool) → E.Entity Service → m ReturnService
serviceToReturnService
    sk'
    uidToBool
    (E.Entity
        key
        (Service
            uid description status date start end
            tracks mixing mastering recording promoKeys))
  = do
    promos ← runDb $ Map.elems <$> P.getMany promoKeys
    pure $ ReturnService (toSToken key) description (uidToBool uid) status
        date start end tracks mixing mastering recording promos
  where
    toSToken = BS8.unpack . tokenize sk' . BSL8.toStrict . encode

