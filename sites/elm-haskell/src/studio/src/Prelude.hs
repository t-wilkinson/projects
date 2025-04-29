-- | Custom Prelude

module Prelude
  ( module Relude
  , module Control.Lens
  , module Web
  , module Json
  , module Error
  ) where

import Relude
import Control.Lens ( (.~), (^.)
                    , view, set)
import Servant.API as Web ( (:>), (:<|>) ((:<|>))
                          , ReqBody, Capture, JSON
                          , Put, Delete, Get, Post )
import Servant.Server.Generic as Web ( AsServerT, genericServe )
  -- can remove asApi and toservant
import Servant.API.Generic as Web ( (:-)
                                  , AsApi, ToServant
                                  )
import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))
import Control.Exception as Error ( throw )
import Servant.Server as Error ( errHTTPCode, errReasonPhrase, errBody, errHeaders )

