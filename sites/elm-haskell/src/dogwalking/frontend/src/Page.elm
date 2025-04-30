module Page exposing (view)

import Browser
import Element exposing (..)


view : (msg -> toMsg) -> { title : String, content : Element msg } -> Browser.Document toMsg
view toMsg { title, content } =
    { title = "" ++ title
    , body =
        [ layout [] <|
            column
                [ width fill ]
                [ map toMsg content
                ]
        ]
    }
