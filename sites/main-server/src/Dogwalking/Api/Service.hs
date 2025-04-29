{-# LANGUAGE TemplateHaskell #-}

module Dogwalking.Api.Service where

import Dogwalking.Db
import Dogwalking.Db.Status (Status(..))
import Core.App (AppS)
import Core.Config (Config, ServerKey, sk)
import Core.Token (SToken, tokenize)
import Quaalude
import Dogwalking.Service (ServiceCharge(..), serviceCharge)
import Dogwalking.Helpers (isAdmin)

import qualified Data.Time           as  Time
import qualified Database.Esqueleto  as  E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON

data ReturnService = ReturnService
    { token ∷ SToken
    , description ∷ String
    , owned ∷ Bool
    , status ∷ Status
    , year ∷ Int, month ∷ Int, day ∷ Int
    , start ∷ Int , end ∷ Int
    } deriving (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromJSON)

data MaybeGetService = MaybeGetService
    { maybeYear ∷ Maybe Integer
    , maybeMonth ∷ Maybe Int
    , maybeDay ∷ Maybe Int
    } deriving (Generic)

JSON.deriveJSON (JSON.defaultOptions { JSON.fieldLabelModifier = fmap toLower . drop 5 }) ''MaybeGetService

data GetService = GetService
    { year ∷ Integer
    , month ∷ Int
    , day ∷ Int
    } deriving (Generic)
      deriving anyclass (ToJSON, FromJSON)

data GetServiceCharge = GetServiceCharge
    { start ∷ Int, end ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data Route r = Route
    { _getSerivceCharge ∷ r
        :- "charge"
        :> ReqBody '[JSON] GetServiceCharge
        :> Post '[JSON] Int
    , _getServicesByDay ∷ r
        :- "day"
        :> ReqBody '[JSON] MaybeGetService
        :> Post '[JSON] [ReturnService]
    , _getServicesByMonth ∷ r
        :- "month"
        :> Description "Get all services in a given month"
        :> ReqBody '[JSON] MaybeGetService
        :> Post '[JSON] [ReturnService]
    } deriving (Generic)

route ∷ Maybe (E.Entity User) → Route AppS
route maybeUser = Route
    getServiceCharge
    (getServicesByDay maybeUser)
    (getServicesByMonth maybeUser)

getServiceCharge ∷ (MonadIO m) ⇒ GetServiceCharge → m Int
getServiceCharge getServiceCharge' = do
    curTime ← liftIO getCurrentTime
    pure $ serviceCharge curTime (toServiceCharge getServiceCharge')
  where
    isTrue = fromMaybe False
    toServiceCharge GetServiceCharge{..} = ServiceCharge Normal 0 0 0 start end

getServicesByDay
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Maybe (E.Entity User)
    → MaybeGetService
    → m [ReturnService]
getServicesByDay maybeUser maybeGetService = do
    sk' ← asks(^.sk)
    curUtctDay ← liftIO $ utctDay <$> getCurrentTime
    let GetService{year, month, day} = getServiceFromMaybe curUtctDay maybeGetService
    services ∷ [E.Entity Service] ← runDb $
        E.select $
            E.from \services → do
                E.where_ $ services E.^. ServiceDay E.==. E.val day
                    E.&&. services E.^. ServiceMonth E.==. E.val month
                    E.&&. services E.^. ServiceYear E.==. E.val (fromIntegral year)
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
    let GetService{year, month} = getServiceFromMaybe curUtctDay maybeGetService
    services ∷ [E.Entity Service] ← runDb $ E.select $ E.from $ \s → do
        E.where_ $ s E.^. ServiceMonth E.==. E.val month
            E.&&. s E.^. ServiceYear E.==. E.val (fromIntegral year)
        pure s
    toReturnService sk' maybeUser services

-- | Default to current date if request does not specify a field.
getServiceFromMaybe ∷ Time.Day → MaybeGetService → GetService
getServiceFromMaybe utctDay MaybeGetService{maybeYear, maybeMonth, maybeDay} =
    let (curYear, curMonth, curDay) = Time.toGregorian utctDay
        (year, month, day) =
            ( fromMaybe curYear maybeYear
            , fromMaybe curMonth maybeMonth
            , fromMaybe curDay maybeDay
            )
    in
    GetService year month day

toReturnService ∷ (MonadReader Config m) ⇒ ServerKey → Maybe (E.Entity User) → [E.Entity Service] → m [ReturnService]
toReturnService sk' Nothing services = pure $ fmap (serviceToReturnService sk' (const False)) services
toReturnService sk' (Just E.Entity{..}) services = do
    isAdmin' ← isAdmin entityVal
    let isValidUser check = check == entityKey || isAdmin'
    pure $ fmap (serviceToReturnService sk' isValidUser) services

serviceToReturnService ∷ ServerKey → (UserId → Bool) → E.Entity Service → ReturnService
serviceToReturnService
    sk'
    uidToBool
    (E.Entity
        key
        (Service uid description status year month day start end))
  =
    ReturnService (toSToken key) description (uidToBool uid) status year month day start end
  where
    toSToken = BS8.unpack . tokenize sk' . BSL8.toStrict . encode

