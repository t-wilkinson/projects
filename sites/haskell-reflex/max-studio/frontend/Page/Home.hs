module Page.Home
    ( vHome
    ) where

import Quaalude
import Common (Test(..))
import Data.Aeson (toJSON, toEncoding)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL (toStrict)

vHome :: View
vHome = do
    test
    "section" ! ("class"=:"relative h-96 overflow-hidden") $ do
        "div" ! ("class"=:"absolute inset-0" <> "style"=:"filter: grayscale(100%) blur(5px); background-position: center; background-size: cover; background-image: url('https://www.sweetwater.com/insync/media/2019/09/5cce6b1f-091919-insync-tourgearsweetwaterstudio-650x340.jpg')") $ blank
        "h1" ! ("class"=:"relative flex flex-col justify-center h-full text-center text-white") $ text "Welcome"
    "section" ! nil $ do
        "h2" ! nil $ text "..."
        "ul" ! ("class"=:"grid-info") $ do
            let attrs = "class"=:"inline rounded-full px-4 py-2 bg-pri text-sec font-black"
                btn txt = domEvent Click . fst <$> (elAttr' "button" attrs $ text txt)
                li = "li" ! "class"=:"col"
            li $ do
                "p" ! nil $ text "View more info about the studio."
                btn "Studio"
            li $ do
                "p" ! nil $ text "Learn more services"
                btn "Services"
            li $ do
                "p" ! nil $ text "View our competitive prices"
                btn "Prices"
    "section" ! nil $ do
        "h2" ! nil $ text "Obi Wan Kenobi"
        "div" ! ("style"=:"position: relative; width: 100%; padding-bottom: 56.25%") $ do
            "iframe" ! ("class"=:"absolute top-0 left-0 w-full h-full border-0" <> "src"=:"https://www.youtube.com/embed/0RHAV51R1Kc") $ blank
        "ul" ! ("class"=:"flex flex-no-wrap justify-center space-x-8 transform transition -translate-y-50 overflow-x-scroll max-w-full w-max absolute") $ do
            videoSuggestions
                "https://i.pinimg.com/originals/a9/c5/72/a9c5729ce6\
                    \0c92a89df50c91a5c52a4f.jpg"
            videoSuggestions
                "https://i2.wp.com/t2.genius.com/unsafe/544x0/https\
                    \%3A%2F%2Fimages.genius.com%2F70a7ef69242915a9c\
                    \d1c3f68031e4c0a.1000x1000x1.jpg?w=640&ssl=1"
            videoSuggestions
                "https://thelodascreative.com/wp-content/uploads/20\
                    \18/01/Issa_21Savage-e1515808321271.png"
            videoSuggestions
                "https://i.pinimg.com/originals/80/9d/a5/809da545a9\
                    \e9d24668512c29c72c0817.jpg"
            videoSuggestions
                "https://d279m997dpfwgl.cloudfront.net/wp/2018/12/JColeKOD.jpg"
            videoSuggestions
                "https://pm1.narvii.com/6874/14c4c75bac68ebb1f53006\
                    \934902b19c42798c1fr1-591-595v2_uhq.jpg"
            videoSuggestions "https://images-cdn.9gag.com/photo/aN1Ajy4_460s.jpg"
            videoSuggestions
                "https://images.contentstack.io/v3/assets/blt1b6090\
                    \5dd65bfb9b/blt34bda9ac79d55d02/5b06688aaaa7eb9\
                    \66fe370e3/tyler-the-creator-flower-boy-album-cover.jpg"

    vFigures
    "section" ! nil $ do
        "h2" ! nil $ text "Testimonials"
        "div" ! nil $ do
            vTestimonial "It was good yo" "the platapus band"
            vTestimonial "It was awesome" "fairy sniffers"
            vTestimonial "10/10 would recommend" "bob dylan"
            vTestimonial "it was quite a cool place" "will"
            vTestimonial "I would definitely go there again" "robert"
            vTestimonial "yeh bro" "bro"

  where
    videoSuggestions :: Text -> View
    videoSuggestions href = --"li" ! ("class"=:"bg-black w-40 h-40 text-sec") $ do
        "img" ! ("class"=:"w-40 h-40" <> "src"=:href) $ blank
        -- text href
    vTestimonial :: Text -> Text -> View
    vTestimonial quote band = "blockquote" ! nil $ do
        text quote
        "footer" ! nil $ "cite" ! nil $ "a" ! nil $ text band

vFigures :: View
vFigures = "section" ! nil $ do
    "h2" ! ("class"=:"text-6xl") $ text "Figures"
    "ul" ! ("class"=:"flex flex-wrap justify-evenly") $ do
        vFigure
            "https://i.pinimg.com/originals/a9/c5/72/a9c5729ce6\
                \0c92a89df50c91a5c52a4f.jpg"
            "This is a description"
            "This is a description"
        vFigure
            "https://i2.wp.com/t2.genius.com/unsafe/544x0/https\
                \%3A%2F%2Fimages.genius.com%2F70a7ef69242915a9c\
                \d1c3f68031e4c0a.1000x1000x1.jpg?w=640&ssl=1"
            "This is a description"
            "another one"
        vFigure
            "https://thelodascreative.com/wp-content/uploads/20\
                \18/01/Issa_21Savage-e1515808321271.png"
            "This is a description"
            "This is a description"
        vFigure
            "https://i.pinimg.com/originals/80/9d/a5/809da545a9\
                \e9d24668512c29c72c0817.jpg"
            "This is a description"
            "Died ghost writes now"
        vFigure
            "https://d279m997dpfwgl.cloudfront.net/wp/2018/12/JColeKOD.jpg"
            "This is a description"
            "I love this one"
        vFigure
            "https://pm1.narvii.com/6874/14c4c75bac68ebb1f53006\
                \934902b19c42798c1fr1-591-595v2_uhq.jpg"
            "This is a description"
            "Bobby"
        vFigure "https://images-cdn.9gag.com/photo/aN1Ajy4_460s.jpg"
            "This is a description"
            "Rest In Jail"
        vFigure
            "https://images.contentstack.io/v3/assets/blt1b6090\
                \5dd65bfb9b/blt34bda9ac79d55d02/5b06688aaaa7eb9\
                \66fe370e3/tyler-the-creator-flower-boy-album-cover.jpg"
            "This is a description"
            "He's a flower boy"

  where
    vFigure :: Text -> Text -> Text -> View
    vFigure src alt caption = "div" ! ("class"=:"") $ do
        "img" ! ("class"=:"w-64 h-64" <> "src"=:src <> "alt"=:alt) $ blank
        text caption

test :: forall t m. MonadWidget t m => m ()
test = "section" ! ("class"=:"text-sec") $ do
    -- let url =  "http://localhost:8001/user/test"
    --     json = Test { testValue = "Value" }
    -- "h1" ! nil $ text "test"
    -- evtStart <- getPostBuild
    -- -- res <- performRequestAsync $ (postJson url json <$ evtStart)
    -- res <- performRequestAsync $ XhrRequest "POST" url
    --     def { _xhrRequestConfig_sendData = TL.toStrict $ encodeToLazyText json
    --       , _xhrRequestConfig_headers = ("content-type"=:"application/json")
    --         } <$ evtStart
    -- let result = (fromMaybe "asfd" . _xhrResponse_responseText) <$> res
    -- dynText =<< holdDyn "wait" result

    pure ()
