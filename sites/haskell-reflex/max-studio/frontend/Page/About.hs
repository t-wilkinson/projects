module Page.About
    ( vAbout
    ) where

import Quaalude

vAbout :: View
vAbout = do
    "h2" ! nil $ text "about"
    vForm

vForm :: View
vForm = "form" ! nil $ "ul" ! nil $ do
    field $ do
        inputElement $ def
            & initialAttributes .~ ("placeholder"=:"test")
        blank
    field $ do
        inputElement $ def
            & initialAttributes .~ ("type"=:"password" <> "placeholder"=:"test")
        blank
    field $ do
        inputElement $ def
            & inputElementConfig_initialValue .~ "hello"
            & initialAttributes .~ ("type"=:"submit")
        blank
    blank

  where
    field = "li" ! nil

