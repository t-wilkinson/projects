{-# LANGUAGE
  OverloadedLabels
#-}

module Db where

import Quaalude
import Core.App (AppT)
import Core.Config (Config, pool)

import Database.Selda
import Database.Selda.SQLite
-- withResource :: MonadBaseControl IO m => Pool a -> (a -> m b) -> m b
import Data.Pool ( withResource )
-- import Database.Selda.PostgreSQL

test :: MonadSelda m => m ()
test = createTable people

-- runDb :: SqlPersistT IO a -> App a
-- runDb query = do
--   pool <- asks (^. pool)
--   liftIO $ runSqlPool query pool

runDb ::
    ( MonadReader r m
    , MonadIO m)
    => IO a
    -> m ()
runDb _ = do
    -- pool <- asks (^.pool)
    pure ()

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)
instance SqlType Pet

data Person = Person
  { name :: Text
  , age  :: Int
  , pet  :: Maybe Pet
  } deriving Generic
instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

main :: IO ()
main = withSQLite "people.sqlite" $ do
-- main = withPostgreSQL ("test" `on` "127.0.0.1") $ do
  createTable people
  insert_ people
    [ Person "Velvet"    19 (Just Dog)
    , Person "Kobayashi" 23 (Just Dragon)
    , Person "Miyu"      10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name :*: person ! #pet)
  liftIO $ print adultsAndTheirPets
