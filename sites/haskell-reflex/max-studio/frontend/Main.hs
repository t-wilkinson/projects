module Main where

import Quaalude
import Page.Home
import Page.About
import Page.Studio

main :: IO ()
main = mainWidgetWithHead vHead vBody

pub var = ("http://localhost:3000/" <> var)

vHead :: View
vHead = do
    "meta" ! ("name"=:"description" <> "content"=:"this is the description") $ blank
    "link" ! ("href"=:pub "favicon.ico.svg" <> "rel"=:"icon" <> "type"=:"image/svg+xml") $ blank
    "link" ! ("href"=:pub "css/output.css" <> "rel"=:"stylesheet" <> "type"=:"text/css") $ blank
    "script" ! ("src"=:pub "js/anime.min.js" <> "async"=:"") $ blank

data Page
  = Home
  | About
  | Studio
  deriving (Show, Eq, Ord)

vBody :: View
vBody = "div" ! "class"=:"gradient" $ do
    -- "script" ! ("type"=:"module") $ text "import anime from 'http://localhost:3000/js/anime.es.js';document.anime = anime;anime({ targets: 'h1', translateX: 250, rotate: '1turn', backgroundColor: '#FFF', duration: 800 }); "
        -- J.eval @Text "anime({ targets: 'h1', translateX: 250, rotate: '1turn', backgroundColor: '#FFF', duration: 800 }); "
    -- tabDisplay "ul css text-sec" "bg-sec text-pri" (1=:("lbl2", text "hi" >> text "lbl2") <> 0=:("lbl1", text "blb1"))
    let views = Home=:vHome <> About=:vAbout <> Studio=:vStudio
    dynKey <- vHeader
    "main" ! "class"=:"space-y-16" <> "id"=:"main"$ do
        selectViewListWithKey dynKey (constDyn views) $ \k dynV dynBool ->
            dyn $ dynBool >>= \b -> if b then dynV else constDyn $ blank
        blank
    vFooter

defClass :: Text -> Text -> Text -> Bool -> Map Text Text
defClass cs true false b = ("class"=:(cs <> " " <> if b then true else false))

-- btns should be links instead?
vHeader :: forall t m. (MonadWidget t m) => m (Dynamic t Page)
vHeader = "header" ! ("class"=:"relative z-10 flex items-center justify-between p-2 shadow-20 bg-sec") $ do
    "a" ! ("href"=:"/") $ do
        "img" ! ("class"=:"" <> "src"=:pub "images/logo.svg" <> "width"=:"80px" <> "height"=:"80px") $ blank
    "h1" ! nil $ text "Klean Studios"

    rec let dynAttrs = defClass "fixed flex flex-col items-center p-8 duration-500 transition-all transform inset-y-0 right-0 bg-sec text-center text-2xl space-y-6" "translate-x-0 shadow-2xl b:blur-screen" "translate-x-full z-10" <$> dynBool
        evts <- elDynAttr "aside" dynAttrs $ do
            "div"!("class"=:"flex flex-col items-center")$ do
                "img" ! ("class"=:"" <> "src"=:pub "images/logo.svg" <> "width"=:"80px" <> "height"=:"80px") $ blank
                "h3" ! nil $ text "Klean Studios"
            "span" ! ("class"=:"w-full h-1 rounded-lg bg-black") $ blank

            evts <- "button"!nil$"ul"!("class"=:"space-y-3 divide-y divide-black") $ traverse ("li" ! ("class"=:"list-none block px-4 hover:text-white"))
                [ fmap (Home <$) $ btn nil $ text "home"
                , fmap (About <$) $ btn nil $ text "about"
                , fmap (Studio <$) $ btn nil $ text "contact"
                ]
            "button" ! ("class"=:"rounded-lg px-2 py-1 border-2 border-black border-solid") $ text "Register"
            holdDyn Home $ leftmost evts

        (evt,_) <- elDynAttr' "ul" menuAttrs $ do
            "li" ! ("class"=:"h-1 w-10 rounded-sm bg-black") $ blank
            "li" ! ("class"=:"h-1 w-10 rounded-sm bg-black") $ blank
            "li" ! ("class"=:"h-1 w-10 rounded-sm bg-black") $ blank
        let clicked = domEvent Click evt
        dynBool <- toggle False clicked
        let menuAttrs = defClass "cursor-pointer space-y-2 z-10" "" "" <$> dynBool
    pure evts

vFooter :: View
vFooter = "footer" ! nil $ do
    blank
