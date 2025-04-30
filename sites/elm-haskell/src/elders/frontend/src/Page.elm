module Page exposing (view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Styles exposing (palette)


view : (subMsg -> msg) -> { title : String, content : Element subMsg } -> Browser.Document msg
view toMsg { title, content } =
    { title = "" ++ title
    , body =
        [ layout [] <|
            column
                [ width fill ]
                [ viewHeader
                , viewNav
                , map toMsg content
                ]
        ]
    }


viewNav : Element msg
viewNav =
    row
        [ spacing 20
        , padding 10
        , Font.color palette.pink
        , Font.size 40
        , Font.extraBold
        , centerX
        ]
        [ link [] { url = "/", label = text "Home" }
        , link [] { url = "/attendance", label = text "Attendance" }
        ]


viewHeader : Element msg
viewHeader =
    row
        [ width fill
        , height (px 50)
        , Border.width 1
        , Border.color palette.black
        ]
        [ text "Site is currently in development."
        ]
