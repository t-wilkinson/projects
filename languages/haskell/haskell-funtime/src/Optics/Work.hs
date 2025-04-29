{-# Language
    TemplateHaskell
#-}

-- |

module Optics.Work where

import           Control.Lens


data ADT = Alpha String | Beta | Gamma Int Int deriving (Show,Eq)

makePrisms ''ADT
