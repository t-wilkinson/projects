module Route exposing (Route(..), fromUrl, link, toString)

import Browser.Navigation as Nav
import Html exposing (Html, a)
import Html.Attributes exposing (class, href)
import Url
import Url.Parser exposing (..)



-- Route


type Route
    = Home
    -- | Login
    -- | Logout
    -- | Studio
    -- | About
    | NotFound


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (parse parser url)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        -- , map Login <| s "login"
        -- , map Logout <| s "logout"
        -- , map Studio <| s "studio"
        -- , map About <| s "about"
        ]


link : Route -> List (Html msg) -> Html msg
link route children =
    a [ href (toString route) ] children


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

--         Login ->
--             "/login"

--         Logout ->
--             "/logout"

--         Studio ->
--             "/studio"

--         About ->
--             "/about"

        _ ->
            ""
