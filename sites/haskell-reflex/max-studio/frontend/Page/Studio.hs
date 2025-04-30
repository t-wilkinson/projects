module Page.Studio
    ( vStudio
    ) where

import Quaalude

vStudio :: View
vStudio = do
    -- viewRequest
    traverse ("section" ! nil)
        [ do
            "h2" ! nil $ text "Prices"
            "ul" ! nil $ do
                vPrice "Recording" "30"
                vPrice "Mixing" "40"
                vPrice "Mastering" "15"
        , "h2" ! nil $ text "Location"
        , "h2" ! nil $ text "Equipment"
        , "h2" ! nil $ text "Occupancy"
        , "h2" ! nil $ text "Amenities"
        ]
    blank

  where
    vPrice tag price = "li" ! nil $ do
        "h3" ! nil $ text tag
        "span" ! nil $ text price
