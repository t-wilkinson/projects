{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Db.User where

import Config ( share' )
import Elm.Derive (defaultOptions, deriveElmDef )
import Database.Persist.TH ( persistLowerCase )
  

data Login = Login
  { loginUsername :: String
  , loginPassword :: String
  } deriving ( Show, Eq, Generic )
instance FromJSON Login
instance ToJSON Login


data Register = Register
  { registerUsername :: String
  , registerPassword :: String
  } deriving ( Show, Eq, Generic )
instance FromJSON Register
instance ToJSON Register


share' [persistLowerCase|
User json
        userUsername String
        UniqueUsername userUsername
        userPassword String
        deriving Show Eq
|]


deriveElmDef defaultOptions ''User
deriveElmDef defaultOptions ''Login
deriveElmDef defaultOptions ''Register
