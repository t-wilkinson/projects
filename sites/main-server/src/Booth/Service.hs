module Booth.Service where

import Quaalude
import Booth.Db
import Booth.Db.Types
import Booth.Helpers (isAdmin)
import Core.Config (Environment(..), Config, sk, env)
import Core.Token (SToken, suntokenize)

import qualified Core.Errors as CE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Database.Esqueleto as E
import qualified Database.Persist as P

data ServiceCharge = ServiceCharge
    { status ∷ Status
    , date ∷ Maybe Date
    , start ∷ Int, end ∷ Int
    , tracks ∷ Int, mixing ∷ Bool, mastering ∷ Bool, recording ∷ Bool
    , promos ∷ [Promo]
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data UpdateService = UpdateService
    { token ∷ SToken
    , start ∷ Int, end ∷ Int
    , description ∷ String
    , mixing ∷ Bool, mastering ∷ Bool, recording ∷ Bool
    , promos ∷ [Promo]
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data AddService = AddService
    { description ∷ String
    , date ∷ Date
    , tracks ∷ Int
    , start ∷ Int
    , end ∷ Int
    , mixing ∷ Bool
    , mastering ∷ Bool
    , recording ∷ Bool
    } deriving (Generic, Show, Eq, Read)
      deriving (ToJSON, FromJSON)

{- Length in minutes -}
serviceLength ∷ Int → Int → Int
serviceLength start end = (wrapEnd start end - start) `div` 60

wrapEnd ∷ Int → Int → Int
wrapEnd start end = if end > start
                       then end
                       else end + 60*60*24

{-# INLINE serviceCharge #-}
serviceCharge ∷ Maybe UTCTime → Rates → ServiceCharge → Double → Double
serviceCharge mayCurTime Rates{..} ServiceCharge{..} acc =
    let validTime = fromMaybe True do
            curTime ← mayCurTime
            Date year month day ← date
            pure (curTime >= UTCTime
                        (fromGregorian (toInteger year) month day)
                        (secondsToDiffTime $ toInteger end))
        charge = acc + case
            ( validTime
            , status == Created      -- 'Status' must be 'Created'
            ) of
            (True, True) →
                let dt = serviceLength start end
                    calculate Rate{..} = (fromIntegral (rateTrack * tracks)) + (fromIntegral (rateDuration * dt)) / 60
                    useBaseRates = not $ or [mastering, mixing, recording]
                in (foldr (\(toggled, rate) a → a + if toggled then calculate rate else 0) 0
                    [ (mastering, ratesMastering)
                    , (mixing, ratesMixing)
                    , (recording, ratesRecording)
                    , (useBaseRates, ratesBase)
                    ])
            _ → 0
    in charge

addService
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → AddService
    → m (Key Service)
addService E.Entity{..} (AddService description date@(Date year month day) tracks start end mixing mastering recording) = do
    curTime ← liftIO $ getCurrentTime
    let utcday = fromGregorian (toInteger year) month day
        diffTime = secondsToDiffTime $ toInteger $ start * 60

    env' ← asks(^.env)
    case env' of
            -- Throw error when service is in past
        Production → when (curTime > UTCTime utcday diffTime) $ throwError CE.badTime
        Remote → when (curTime > UTCTime utcday diffTime) $ throwError CE.badTime
        Development → pure ()

    serviceId ← getOverlappingServices Nothing year month day start (wrapEnd start end) >>= \case
        [] → do
            runDb $ E.insert $
                Service entityKey description NotConfirmed date
                start end tracks
                mixing mastering recording
                []
        _ →
            throwError CE.invalidService
    pure serviceId

setServiceProcessing
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
   ⇒ E.Entity User
   → SToken
   → m ()
setServiceProcessing entityUser token = do
    sk' ← asks(^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    case maybeKey of
        Nothing → throwError CE.token
        Just key → do
            validateOwnership entityUser key
            -- Validate
            curTime ← liftIO getCurrentTime
            service@Service{..} ← (runDb $ E.get key) >>= \case
                Nothing → throwError CE.serviceDNE
                Just service → pure service
            let Date{..} = serviceDate
                isValid = isServiceInFuture service curTime

            if isValid
               then do
                   services ← getOverlappingServices (Just key) dateYear dateMonth dateDay
                       serviceStart (wrapEnd serviceStart serviceEnd)
                   case services of
                       [] → runDb $ P.update key [ServiceStatus P.=. Processing]
                       _ → throwError CE.serviceOverlapping
               else throwError CE.serviceInPast
            pure ()

deleteService
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
   ⇒ E.Entity User
   → SToken
   → m ()
deleteService entityUser token = do
    sk' ← asks(^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    case maybeKey of
        Nothing → throwError CE.token
        Just key → do
            validateOwnership entityUser key
            -- Validate
            curTime ← liftIO getCurrentTime
            isValid ← (runDb $ E.get key) >>= \case
                Nothing → throwError CE.serviceDNE
                Just service → pure $ isServiceInFuture service curTime
            if isValid
               then runDb $ P.update key [ServiceStatus P.=. Removed]
               else throwError CE.serviceInPast
            pure ()

updateService
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
   ⇒ E.Entity User
   → SToken
   → UpdateService
   → m ()
updateService entityUser token (UpdateService key start end description mixing mastering recording promos) = do
    sk' ← asks (^.sk)

    -- Validate token
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    key' ← case maybeKey of
        Nothing → throwError CE.token
        Just key'' → pure key''

    -- Get service that corresponds to key
    service ← (runDb $ E.get key') >>= \case
        Nothing → throwError CE.serviceDNE
        Just service → pure service

    services ←
        let Service{..} = service
            Date{..} = serviceDate
        in getOverlappingServices (Just key') dateYear dateMonth dateDay serviceStart (wrapEnd serviceStart serviceEnd)

    -- Validate
    curTime ← liftIO getCurrentTime
    let isValid = case services of
            [] → isServiceInFuture service curTime
            _ → False

    -- Update Service
    if isValid
       then do
           validateOwnership entityUser key'
           runDb $ P.update key'
               [ ServiceStart P.=. start
               , ServiceEnd P.=. end
               , ServiceDescription P.=. description
               ]
       else throwError CE.invalidService
    pure ()

isServiceInFuture ∷ Service → UTCTime → Bool
isServiceInFuture service curTime =
    let Date{..} = serviceDate service
        end = toInteger $ serviceEnd service
    in curTime <= UTCTime
                (fromGregorian (toInteger $ dateYear) dateMonth dateDay)
                (secondsToDiffTime end)

validateOwnership
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
   ⇒ E.Entity User
   → Key Service
   → m ()
validateOwnership E.Entity{..} serviceKey = do
    maybeService ← runDb $ E.get serviceKey

    isAdmin' ← isAdmin entityVal
    let validate service = serviceUId service == entityKey || isAdmin'
    case maybe False validate maybeService of
      True → pure ()
      _ → throwError err404

getOverlappingServices
    ∷ ∀ m.
        ( MonadIO m
        , MonadError ServerError m
        , MonadReader Config m
        )
    ⇒ Maybe (Key Service)
    → Int → Int → Int
    → Int → Int
    → m [E.Entity Service]
getOverlappingServices mayServiceKey year month day start end = do
    unless (start < (wrapEnd start end)) $ throwError CE.backwardsTimes
    let checks s =
            -- Don't check processing because it is hard to check when payment failed
                 (s E.^. ServiceStatus E.==. E.val Created
            E.||. s E.^. ServiceStatus E.==. E.val Processing
            E.||. s E.^. ServiceStatus E.==. E.val PaymentProcessing
            )
            -- is the service on the same day?
            E.&&. s E.^. ServiceDate E.==. E.val (Date year month day)
            -- ends between an existing service?
            E.&&.((s E.^. ServiceStart E.<. E.val end
            E.&&. s E.^. ServiceEnd E.>=. E.val end)
            -- starts in-between an existing service?
            E.||. (s E.^. ServiceStart E.<=. E.val start
            E.&&. s E.^. ServiceEnd E.>. E.val start)
            -- exists surrounding an existing service? (starts before, ends after)
            E.||. (s E.^. ServiceStart E.>. E.val start
            E.&&. s E.^. ServiceEnd E.<. E.val end)
                   )

    services ← runDb $ E.select $ E.from \s → do
        E.where_ $ checks s E.&&. case mayServiceKey of -- True if keys are equal, or input is Nothing
                                      Just key → s E.^. ServiceId E.!=. E.val key
                                      Nothing → E.val True
        pure s
    pure services
