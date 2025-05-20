{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Booth.Api.Admin where

import Quaalude
import Core.Config (Config, sk)
import Core.App (AppS)
import Booth.Db
import Booth.Db.Types
import Data.Time.Calendar (Day, toGregorian)
import Core.Token (SToken, tokenize, suntokenize)
import qualified Core.Errors as CE

import Database.Esqueleto hiding ((^.))
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

data Return = Return
    { userId ∷ UserId
    , user ∷ User
    , serviceId ∷ ServiceId
    , service ∷ Service
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data AddProject = AddProject
    { group ∷ String
    , alt ∷ String
    , href ∷ String
    , src ∷ String
    , order ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data ReturnProject = ReturnProject
    { token ∷ SToken
    , group ∷ String
    , alt ∷ String
    , href ∷ String
    , src ∷ String
    , order ∷ Int
    } deriving (Generic)
      deriving (ToJSON, FromJSON)

data Route r = Route
    { _validate ∷ r
        :- "validate"
        :> Description "User is admin iff this function is called"
        :> Get '[JSON] Bool
    , _search ∷ r
        :- "search"
        :> QueryParam "name" String
        :> QueryParam "email" String
        :> QueryParam' '[Lenient, Optional] "date" Day
        :> Get '[JSON] [Return]
    , _deny ∷ r :- "deny"       :> Capture "deny" Bool :> ReqBody '[JSON] ServiceId :> Put '[JSON] ()
    , _getRates ∷ r :- "rates"  :> Get '[JSON] Rates
    , _getRateFee ∷ r :- "rates" :> "fee" :> Get '[JSON] Rate
    , _setRates ∷ r
        :- "rates"
        :> ReqBody '[JSON] Rates
        :> Put '[JSON] ()
    , _setProjects ∷ r :- "projects" :> ReqBody '[JSON] AddProject :> Post '[JSON] ()
    , _putProject ∷ r
        :- "projects"
        :> Capture "project" SToken
        :> ReqBody '[JSON] AddProject
        :> Put '[JSON] ()
    , _deleteProject ∷ r
        :- "projects"
        :> Capture "project" SToken
        :> Delete '[JSON] ()
    } deriving (Generic)

route ∷ Route AppS
route = Route
    (pure True)
    search
    deny
    getRates
    getRateFee
    setRates
    setProjects
    putProject
    deleteProject

search
    ∷ ∀  m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Maybe String
    → Maybe String
    → Maybe (Either Text.Text Day)
    → m [Return]
search mayName mayEmail mayDay = do
    let
        name = fromMaybe "" mayName
        email = fromMaybe "" mayEmail
        mayDate = do
            eitherDay ← mayDay
            day ← either (const Nothing) Just eitherDay
            let (year, month, day') = toGregorian day
            pure $ Date (fromIntegral year) month day'

    matches ← runDb $ E.select $ E.from \(user,service) →
        let surround string = E.lower_ $ foldr (\x a → (%) ++. val x ++. a) (%) $ words string
            search1 = (E.lower_ (user E.^. UserName) `like` surround name)
                &&. (E.lower_ (user E.^. UserEmail) `like` surround email)
                &&. (user E.^. UserId ==. service E.^. ServiceUId)
        in case mayDate of
            Just date → do
                E.where_ $
                    (service E.^. ServiceDate) E.==. E.val date
                    &&. search1
                pure (user,service)
            Nothing → do
                where_ $ search1
                pure (user,service)

    pure $ (\((E.Entity uid user), (E.Entity sid service)) →
        Return uid user sid service) <$> matches

deny
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ Bool
    → ServiceId
    → m ()
deny bool serviceid = do
    let status = if bool then Removed else Created
    runDb $ P.update serviceid [ServiceStatus P.=. status]
    pure ()

getRates
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ m Rates
getRates = do
    (runDb $ P.selectFirst [] []) >>= \case
        Just E.Entity{..} → pure entityVal
        Nothing → throwError CE.noRates

getRateFee
    ∷ ∀  m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ m Rate
getRateFee = do
    (runDb $ P.selectFirst [] []) >>= \case
        Just E.Entity{..} → pure $ ratesService entityVal
        Nothing → throwError CE.noRates

setRates
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ Rates
    → m ()
setRates rates = do
    (runDb $ P.selectFirst [] []) >>= \case
        Just E.Entity{..} → runDb $ P.replace entityKey rates
        Nothing → throwError CE.noRates
    pure ()

getProjects
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ m [ReturnProject]
getProjects = do
    sk' ← asks (^.sk)
    projects ← runDb $ P.selectList [] []
    let toSToken = BS8.unpack . tokenize sk' . BSL8.toStrict . encode
        toReturnProject E.Entity{..} =
            let Project{..} = entityVal
            in ReturnProject (toSToken entityKey) projectGroup projectAlt projectHref projectSrc projectOrder
    pure $ toReturnProject <$> projects

setProjects
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        )
    ⇒ AddProject
    → m ()
setProjects AddProject{..} = do
    _ ← runDb $ P.insert $ Project group alt href src order
    pure ()

putProject
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ SToken
    → AddProject
    → m ()
putProject stoken AddProject{..} = do
    sk' ← asks (^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk' stoken
    key ← case maybeKey of
        Nothing → throwError CE.token
        Just key → pure key
    runDb $ P.update key [ ProjectGroup P.=. group
                         , ProjectAlt P.=. alt
                         , ProjectHref P.=. href
                         , ProjectSrc P.=. src
                         , ProjectOrder P.=. order
                         ]

deleteProject
    ∷ ∀ m.
        ( MonadIO m
        , MonadReader Config m
        , MonadError ServerError m
        )
    ⇒ SToken
    → m ()
deleteProject stoken = do
    sk ← asks (^.sk)
    let maybeKey = decode $ BSL8.pack $ suntokenize sk stoken
    key ∷ Key Project ← case maybeKey of
        Nothing → throwError CE.token
        Just key → pure key
    runDb $ P.delete key
    pure ()
