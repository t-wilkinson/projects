{-# Language
    TemplateHaskell
#-}

-- | Its some folds!!!

module Optics.Folds where

import           Control.Lens
import           Data.Set                      as S
import           Data.Char                      ( toUpper )


data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

instance Semigroup Role where
  r1 <> r2 = r1
instance Monoid Role where
  mempty = FirstMate


data CrewMember =
  CrewMember { _name :: String
             , _role :: Role
             , _talents :: [String]
             } deriving (Show, Eq, Ord)

instance Semigroup CrewMember where
  (CrewMember n1 r1 t1) <> (CrewMember n2 r2 t2) =
    CrewMember (n1 <> n2) r1 (t1 <> t2)

instance Monoid CrewMember where
  mempty = CrewMember "" FirstMate []

makeLenses ''CrewMember


roster :: S.Set CrewMember
roster = S.fromList
  [ CrewMember "Grumpy Roger"     Gunner       ["Juggling", "Arbitrage"]
  , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
  , CrewMember "Salty Steve"      PowderMonkey ["Origami"]
  , CrewMember "One-eyed Jack"    Navigator    []
  ]

testFolds :: IO ()
testFolds = do
  -- view, which deals with lenses, can return ONLY one value
  -- as a result, must make data types instances of monoid to
  -- provide <> and mempty
  print $ roster ^. folded . role
  print $ roster ^.. folded . role
  print $ sizes ^.. (folded . folded . folded) == "hiboobye"
  where sizes = [(3, "hi"), (88, "boo"), (303, "bye")] :: [(Int, String)]


-- | Crew update
newtype Name =
  Name { getName :: String
       } deriving Show

data ShipCrew =
  ShipCrew { _shipName :: Name
           , _captain :: Name
           , _firstMate :: Name
           , _conscripts :: [Name]
           } deriving (Show)

makeLenses ''ShipCrew

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew = [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

crewNames :: Fold ShipCrew Name
crewNames =
  folding (\s -> s ^.. captain <> s ^.. firstMate <> s ^.. conscripts . folded)

crewNames' :: Fold ShipCrew Name
crewNames' =
  folding (\s -> s ^.. captain <> s ^.. firstMate <> s ^. conscripts)

myCrew :: ShipCrew
myCrew = ShipCrew { _shipName   = Name "Purple Pearl"
                  , _captain    = Name "Grumpy Roger"
                  , _firstMate  = Name "Long-John Bronze"
                  , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
                  }

testMyCrew :: IO ()
testMyCrew = do
  print $ Name "robert" ^. to getName
  print $ Name "bob" ^. to getName . to (fmap toUpper)
  print $ Name "lorry" ^. to (fmap toUpper . getName)
  print $ myCrew ^.. folding collectCrewMembers . to getName
