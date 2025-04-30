{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Db.Content where

import Config ( share' )
import Elm.Derive (defaultOptions, deriveElmDef )
import Database.Persist.TH ( persistLowerCase )

share' [persistLowerCase|
        Content json
            contentHeading String
            contentBody String
            deriving Show Eq Typeable
|]

deriveElmDef defaultOptions ''Content
