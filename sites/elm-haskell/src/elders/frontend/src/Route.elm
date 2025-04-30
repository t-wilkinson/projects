module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, parse, s, string, top)


type Route
    = Home
    | Attendance
    | NotFound


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map Attendance (s "attendance")
        ]
