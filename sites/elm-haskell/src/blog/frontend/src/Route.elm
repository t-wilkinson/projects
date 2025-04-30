module Route exposing (Route(..), fromUrl, home, toUrl)

import Element exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s, string, top)



---- ROUTE ----


type Route
    = Home
    | Post String
    | EditPost String
    | NewPost
    | Login
    | Logout
    | Register



---- URL ----


home : String
home =
    "/"


toUrl : Route -> { url : String, label : Element msg }
toUrl route =
    let
        href url label =
            { url = String.join "/" url
            , label = text label
            }
    in
    case route of
        Home ->
            href [ "/" ] "Home"

        Post title ->
            href [ "blog", title ] "View Post"

        EditPost title ->
            href [ "editor", title ] "Edit Post"

        NewPost ->
            href [ "blog" ] "Create Post"

        Login ->
            href [ "login" ] "Login"

        Logout ->
            href [ "logout" ] "Logout"

        Register ->
            href [ "register" ] "Register"


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map Post (s "blog" </> string)
        , Parser.map EditPost (s "editor" </> string)
        , Parser.map NewPost (s "blog")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Register (s "register")
        ]
