module Booth.Api.Projects where

import Quaalude
import Core.Config (Config, sk)
import Core.App (AppS)
import Booth.Db
import Core.Token (SToken, tokenize)

import Database.Esqueleto hiding ((^.))
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

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
    { _getProjects ∷ r :- "projects" :> Get '[JSON] [ReturnProject]
    } deriving (Generic)

route ∷ Route AppS
route = Route
    getProjects

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
