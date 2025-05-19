module Dogwalking.Api.ServiceProtected where

import Quaalude
import Core.App (AppS)
import Core.Config (Config, sk)
import Core.Token (SToken, suntokenize)
import Dogwalking.Helpers (isAdmin)
import Dogwalking.Db
import Dogwalking.Db.Status (Status(..))

import qualified Core.Errors as CE
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Database.Esqueleto as E
import qualified Database.Persist as P

data AddService = AddService
    { description ∷ String
    , year ∷ Int
    , month ∷ Int
    , day ∷ Int
    , start ∷ Int
    , end ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data UpdateService = UpdateService
    { token ∷ SToken
    , start ∷ Int, end ∷ Int
    , description ∷ String
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data Route r = Route
    { _addService ∷ r
        :- ReqBody '[JSON] AddService
        :> Post '[JSON] ()
    , _updateService ∷ r
        :- Capture "token" SToken
        :> ReqBody '[JSON] UpdateService
        :> Put '[JSON] ()
    , _removeService ∷ r
        :- Capture "token" SToken
        :> Delete '[JSON] ()
    } deriving (Generic)

route ∷ E.Entity User → Route AppS
route user = Route
    (addService user)
    (updateService user)
    (removeService user)

addService
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ E.Entity User
    → AddService
    → m ()
addService E.Entity{..} (AddService description year month day start end) = do
    curTime ← liftIO $ getCurrentTime
    let utcday = fromGregorian (fromIntegral year) month day
        diffTime = secondsToDiffTime $ fromIntegral $ start * 60
    isAdmin' ← isAdmin entityVal
    when (curTime > UTCTime utcday diffTime && not (isAdmin')) $ throwError CE.badTime
    _ ← getOverlappingServices year month day start end >>= \case
        [] → runDb $ E.insert $
                Service entityKey description Normal year month day start end
        _ → throwError CE.invalidService
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
updateService entityUser token (UpdateService key start end description) = do
    sk' ← asks (^.sk)

    -- Validate token
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    key' ← case maybeKey of
        Nothing → throwError CE.token
        Just key'' → pure key''

    -- Get service that corresponds to key
    services ← (runDb $ E.get key') >>= \case
        Nothing → throwError CE.serviceDNE
        Just (Service _ _ _ year month day _ _) → getOverlappingServices year month day start end

    -- Validate
    let isValid = case services of
            [] → False
            (service:_) → E.entityKey service == key'

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

removeService
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
   ⇒ E.Entity User
   → SToken
   → m ()
removeService entityUser token = do
    sk' ← asks(^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' token
    case maybeKey of
        Nothing → throwError CE.token
        Just key → do
            validateOwnership entityUser key
            runDb $ P.delete key
            pure ()

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
      False → throwError err404

getOverlappingServices
    ∷ ∀ m.
        ( MonadIO m
        , MonadError ServerError m
        , MonadReader Config m
        )
    ⇒ Int → Int → Int
    → Int → Int
    → m [E.Entity Service]
getOverlappingServices year month day start end = do
    unless (start < end) $ throwError err409 { errBody = "Start time must precede end time." }
    runDb $ E.select $ E.from $ \(s ∷ E.SqlExpr (E.Entity Service)) → do
        E.where_ $
            -- is the service on the same day?
                  (s E.^. ServiceYear E.==. E.val year
            E.&&. s E.^. ServiceMonth E.==. E.val month
            E.&&. s E.^. ServiceDay E.==. E.val day)
            E.&&.
            -- ends between an existing service?
                 (s E.^. ServiceStart E.<. E.val end
            E.&&. s E.^. ServiceEnd E.>=. E.val end
            -- starts in-between an existing service?
            E.||. s E.^. ServiceStart E.<=. E.val start
            E.&&. s E.^. ServiceEnd E.>. E.val start
            -- exists surrounding an existing service?
            E.||. s E.^. ServiceStart E.<. E.val start
            E.&&. s E.^. ServiceEnd E.>. E.val end)
        pure s


