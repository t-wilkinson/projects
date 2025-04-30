{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Db.Studio where

import Config ( share' )
import Elm.Derive (defaultOptions, deriveElmDef )
import Database.Persist.TH ( persistLowerCase )


share'
    [persistLowerCase|
        StudioRequest json
            date String
            startTime Int
            endTime Int
            deriving Show Eq Typeable

   |]


-- UniqueBeatTag beatID tagID

deriveElmDef defaultOptions ''StudioRequest

