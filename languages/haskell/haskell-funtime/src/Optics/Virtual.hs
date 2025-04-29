{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# Language
  NamedFieldPuns -- pattern match by name of record
  , RecordWildCards -- pattern match with wild card
  , TemplateHaskell
  , FlexibleContexts
#-}

module Optics.Virtual where

import           Control.Lens


data Person =
  Person { _name :: String
         } deriving (Show)


setNamePerson :: Person -> String -> Person
setNamePerson person name = person { _name = name }

getNamePerson :: Person -> String
getNamePerson Person { _name } = _name

namePerson :: Lens' Person String
namePerson = lens getNamePerson setNamePerson


data Alien =
  Alien { _alienName :: String
        , _alienCelsius :: Float
        } deriving (Show)


celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)


makeFields ''Alien

fahrenheit :: Lens' Alien Float
fahrenheit = lens getter setter
 where
  getter = celsiusToFahrenheit . view celsius
  setter temp f = set celsius (fahrenheitToCelsius f) temp

