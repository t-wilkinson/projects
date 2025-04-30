{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Db.Beat where

import Config ( share' )
import Elm.Derive (defaultOptions, deriveElmDef )
import Database.Persist.TH ( persistLowerCase )


share'
    [persistLowerCase|
        Beat json
            beatName String
            beatIcon String
            deriving Show Eq Typeable
        Tag json
            tagName String
            deriving Show Eq Typeable
        BeatTag
            beatID BeatId
            tagID TagId
            UniqueBeatTag beatID tagID

   |]
      

deriveElmDef defaultOptions ''Beat
deriveElmDef defaultOptions ''Tag
