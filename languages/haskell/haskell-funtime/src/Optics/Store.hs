{-# Language
    TemplateHaskell
#-}

-- | A store to mess around with lens/prisms

module Optics.Store where

import           Control.Lens


-- | Lemons!!
data ProducePrices =
  ProducePrices { _limePrice :: Float
                , _lemonPrice :: Float
                } deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
 where
  getter = _limePrice
  setter prices price =
    let limep  = max 0 price
        lemonp = view lemonPrice prices
    in  prices { _limePrice  = limep
               , _lemonPrice = min (limep + 0.5) $ max (limep - 0.5) lemonp
               }


lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
 where
  getter = _lemonPrice
  setter prices price =
    let limep  = view lemonPrice prices
        lemonp = max 0 price
    in  prices { _lemonPrice = lemonp
               , _limePrice  = min (lemonp + 0.5) $ max (lemonp - 0.5) limep
               }


-- | Sum Types
data FoodGroup a
  = Dairy a Produce
  | Protein
  deriving (Show)


-- | Product Types
data Produce =
  Produce { _name :: String
          } deriving (Show)

data Store =
  Store { _storeProduce :: Produce
        , _address :: String
        , _foodGroup :: FoodGroup String
        } deriving (Show)


address' :: Lens' Store String
address' f (Store x1 x2 x3) = fmap (\y -> Store x1 y x3) (f x2)

makeClassy ''Produce
makeClassy ''Store
makePrisms ''FoodGroup

testStore :: IO ()
testStore = do
  print $ set name "bread" (Produce "bob")
  print $ Produce "bob" & name .~ "bread"
  print
    $  Store { _storeProduce = Produce "bread"
             , _address      = "6402 Tsidale"
             , _foodGroup    = Dairy "allergic" (Produce "skim please")
             }
    &  storeProduce
    .  name
    %~ ("wheat " ++)
  print
    $  Store { _storeProduce = Produce "eggs"
             , _address      = "4223 fsda"
             , _foodGroup    = Dairy "milk" (Produce "skim milk")
             }
    ^. foodGroup
    ^? _Dairy

{-
declareLenses [d|
               data Produce =
                  Produce { name :: String
                          } deriving (Show)
                |]
-}
