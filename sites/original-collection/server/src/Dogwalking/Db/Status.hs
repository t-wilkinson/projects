{-# Language
  TemplateHaskell
#-}

module Dogwalking.Db.Status where

import Quaalude
import Database.Persist.TH

data Status
    = Normal
    | Cancled
    deriving (Show, Read, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

derivePersistField "Status"


