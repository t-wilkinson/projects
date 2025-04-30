{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Reflex
import Reflex.Dom
import Data.Monoid ((<>))
import Data.Functor ( ($>), void )
import Data.Maybe ( fromMaybe )
import Data.Time ( getCurrentTime )
import Data.Witherable ( Filterable )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T


data Page
    = PageData
    | PageError

main :: IO ()
main = mainWidgetWithHead vHead vBody

vHead :: MonadWidget t m => m ()
vHead = do
    w "link" ("href"=:"http://localhost:3005/css/output.css" <> "rel"=:"stylesheet" <> "type"=:"text/css") $ blank

type View t m = MonadWidget t m => m ()

vCards :: MonadWidget t m => m ()
vCards = w "div cards" nil $ do
    vCard nil $ do
        text "one"
    vCard nil $ do
        text "second"
    vCard nil $ do
        w "div" nil $ text "1"
        w "div" nil $ text "2"
        w "div" nil $ text "3"

vCard :: MonadWidget t m => Map Text Text -> m () -> m ()
vCard attrs = w "div card" attrs

vBody :: MonadWidget t m => m ()
vBody = do
    vCards
    blank

test :: View t m
test = do
    w "h1" nil $ text "test"

vTime :: forall t m. ( MonadIO m, MonadWidget t m ) => m ()
vTime = do
    el "h2" $ text "Time"
    t <- liftIO getCurrentTime
    ti <- tickLossy 1 t
    let time = (T.pack . show. _tickInfo_lastUTC) <$> ti
    dynText =<< holdDyn "No time" time

vInputs :: forall t m. MonadWidget t m => m ()
vInputs = divClass "inputs" $ do
    -- tagPromptlyDyn
    rec e <- btn "" nil $ text "dynButton"
        ie <- inputElement $ def
            & initialAttributes .~ ( "placeholder"=:"Max 20 chars" <> "maxlength"=:"20" )
        let eText = tagPromptlyDyn (value ie) e
        dynText =<< holdDyn "text" eText

    br
    -- Input Element with Keypress
    rec ie <- inputElement def
        let e = keypress Enter ie
            eText = tagPromptlyDyn (value ie) e
        dynText =<< holdDyn "" eText
        ti <- inputElement $ def
            & inputElementConfig_setValue .~ (tagPromptlyDyn (value ie) e)
            & modifyAttributes .~ ( e $> "placeholder"=:Just"thisisme" )
        display $ traceDyn "inputelement" (value ie)

    br
    -- Color input
    rec
        let label :: T.Text -> m (InputElement EventResult (DomBuilderSpace m) t)
            label txt = do
                el "label" $ text txt
                inputElement $ def
                    & initialAttributes .~ ( "type"=:"number" <> "max"=:"255" <> "min"=:"0" )

            styleMap :: T.Text -> T.Text -> T.Text -> Map.Map AttributeName (Maybe T.Text)
            styleMap r g b = Map.fromList [
                ( AttributeName (Just "bob") "style"
                , Just (mconcat [ "background-color: rgb(",r,",",g,",",b,")" ])
                ) ]

        g <- label "green"
        b <- label "blue"
        r <- label "red"

        tae <- textAreaElement $ def
            & modifyAttributes .~ updated ( styleMap <$> value r <*> value g <*> value b )

    br
    -- dropdown
    rec dd <- dropdown "1" (constDyn $ Map.fromList [("1","one"),("2","two"),("3","three")]) def
        dynText $ value dd

    br
    -- Checkbox
    rec inputElement $ def
            & initialAttributes .~ ( "type"=:"checkbox" )
        text "checkbox label"

    br
    -- Input Range
    rec d <- rangeInput def

    br
    -- Select element
    selectElement def (text "selectelement")
    textNode def
    element "element" def (text "element text")

    blank



vButtons :: forall t m. MonadWidget t m => m ()
vButtons = divClass "buttons" $ do
    -- click
    br
    rec mBool <- toggle True (click e)
        (e,_) <- elDynAttr' "button" (bAttrs <$> mBool) $ text "Login"

    br
    -- leftmost
    rec
        count <- foldDyn (+) 0 $ leftmost [ -1 <$ ed, 1 <$ ei ]
        display $ ffor count (T.pack . ("leftmost" <>) . show)
        e <- btn "" nil $ text "+"
        let ei = traceEventWith (const "button pushed") e
        ed <- btn "" nil $ text "-"

    br

    -- mergeWith
    rec count <- foldDyn (+) 0 $ mergeWith (+) [1 <$ ei, -1 <$ ed]
        display $ ffor count (T.pack . ("merge" <>) . show)
        ei <- btn "" nil $ text  "+"
        ed <- btn "" nil $ text  "-"

    br
    -- leftmost
    rec dynNum <- foldDyn ($) 0 $ leftmost
            [ (+ 1) <$ eI
            , (+ (-1)) <$ eD
            , const 0 <$ eZ
            ]
        (eI,eD,eZ) <- (,,)
            <$> (btn "" nil $ text  "+")
            <*> (btn "" nil $ text  "-")
            <*> (btn "" nil $ text  "0")
        el "div" $ display dynNum

    blank

    where
        bAttrs :: Bool -> Map.Map T.Text T.Text
        bAttrs b = "style" =: ("background-color:" <> color)
            where color
                    | b = "#00ffff"
                    | otherwise = "#ffffff"

vMeteo :: forall t m. ( MonadIO m, MonadWidget t m ) => m ()
vMeteo = el "div" $ do
    el "h2" $ text "Swiss Meteo Data (raw version)"
    text "Choose station: "
    dd <- dropdown "BER" (constDyn stations) def
    -- Build and send the request
    evStart <- getPostBuild
    let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
    evRsp <- performRequestAsync $ buildReq <$> evCode
    -- Display the whole response
    el "h5" $ text "Response Text:"
    let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
    dynText =<< holdDyn "" evResult
    -- Display error/data page depending on result of request
    let (evOk, evErr) = checkXhrRsp evRsp
    dynPage <- foldDyn ($) PageData $ leftmost [const PageData <$ evOk, const PageError <$ evErr]
    return ()

  where
    buildReq :: T.Text -> XhrRequest ()
    buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

    checkXhrRsp :: Filterable f => f XhrResponse -> (f XhrResponse, f XhrResponse)
    checkXhrRsp evRsp = (evOk, evErr)
      where
        evOk = ffilter (\rsp -> _xhrResponse_status rsp == 200) evRsp
        evErr = ffilter (\rsp -> _xhrResponse_status rsp /= 200) evRsp

    stations :: Map.Map T.Text T.Text
    stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]



click :: HasDomEvent t a 'ClickTag => a -> Event t (DomEventType a 'ClickTag)
click = domEvent Click

br :: MonadWidget t m => m ()
br = el "br" blank

type Attr = Map Text Text

nil :: Attr
nil = mempty

btn :: MonadWidget t m => Text -> Attr -> m a -> m (Event t ())
btn c attrs ma = domEvent Click . fst <$> w' ("button " <> c) attrs ma

h2 :: MonadWidget t m => m () -> m ()
h2 = w "h2 text-pri text-6xl" nil

w :: MonadWidget t m => Text -> Attr -> m a -> m a
w = w_ elAttr

w' :: MonadWidget t m => Text -> Attr -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
w' = w_ elAttr'

w_ :: (Text -> Attr -> m a -> m b) -> Text -> Attr -> m a -> m b
w_ f string attrs child =
    let (x:xs) = T.split (==' ') string
    in
    case xs of
      [] -> f x attrs child
      xs -> f x ("class"=:T.unwords xs <> attrs) child


