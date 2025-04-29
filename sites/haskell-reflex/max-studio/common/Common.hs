{-# Language
  DerivingStrategies
, DeriveAnyClass
, DeriveGeneric
#-}

module Common where

import Quaalude

data User = User
    { name :: String
    , password :: String
    } deriving (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)

data Test = Test
    { testValue :: String
    } deriving (Show, Eq, Generic)
      deriving anyclass (FromJSON, ToJSON)
