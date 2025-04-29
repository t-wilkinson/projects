module Elders.Database where


import GHC.Generics
import Elm.Derive ( defaultOptions, deriveBoth )
import Control.Lens (makeLenses)
import Control.Monad.Reader (ask)
import qualified Control.Monad.State as S

import Data.IntMap (IntMap, Key)
import Data.Typeable
import qualified Data.IntMap as IntMap

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Acid
import Data.SafeCopy
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.List


---- Types ----

type DB = DaysDB
type DaysDB = AcidState Days
type YMD = (Integer,Int,Int)


newtype People = People [Person]
    deriving (Show, Ord, Eq, Typeable, Generic)


newtype Days = Days [Day]
    deriving (Show, Ord, Eq, Typeable, Generic)


data Person = Person 
    { name :: String
    , icon :: String
    } deriving (Show, Ord, Eq, Generic)


data Day = Day (Integer,Int,Int) [Person]
    deriving (Show, Ord, Eq, Generic)


deriveBoth defaultOptions ''Person
deriveBoth defaultOptions ''Day
deriveBoth defaultOptions ''Days
deriveBoth defaultOptions ''People

$(deriveSafeCopy 0 'base ''Person)
$(deriveSafeCopy 0 'base ''People)
$(deriveSafeCopy 0 'base ''Days)
$(deriveSafeCopy 0 'base ''Day)

makeLenses ''People
makeLenses ''Days



---- Functions ----


getDays :: Query Days Days
getDays = do
    Days days <- ask
    return $ Days days


getPeople :: Query People People
getPeople = do
    People people <- ask
    return $ People people


postPeople :: YMD -> [Person] -> Update Days ()
postPeople date ps = do
    Days days <- S.get
    S.put $ Days $ Day date ps:days



$(makeAcidic ''People ['getPeople])
$(makeAcidic ''Days ['getDays, 'postPeople])



openDatabase :: IO DB
openDatabase = openLocalStateFrom "../database/days/" (Days [])
