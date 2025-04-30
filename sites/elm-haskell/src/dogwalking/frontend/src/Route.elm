module Route exposing (Route(..), fromUrl)

import Browser.Navigation as Nav
import Url
import Url.Parser exposing (..)



---- Route ----


type Route
    = Home
    | NotFound


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (parse route url)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        ]
