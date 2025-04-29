{-# Language
  DeriveGeneric
, DeriveAnyClass
, DerivingStrategies
#-}

module Common where

import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Database.Selda ( SqlRow )
import Data.Text ( Text )

data User = User
    { username :: Text
    , password :: Text
    } deriving ( Eq, Show, Read, Generic )
      deriving anyclass ( ToJSON, FromJSON, SqlRow )
