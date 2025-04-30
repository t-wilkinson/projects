-- | Custom Prelude

module Prelude
  ( module Relude
  , module Control.Lens
  , module Web
  ) where

import Relude
import Control.Lens ((.~),(^.))
import Servant.API as Web ((:>), Capture, JSON, Put, Delete, Get, Post, NoContent (NoContent))

