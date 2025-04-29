module Page exposing (Page(..), User(..), getUsername, toFlag, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, Error, string)
import Json.Encode exposing (Value)
import Page.Editor as Editor
import Page.Home as Home
import Page.Login as Login
import Page.Post as Post
import Page.Register as Register
import Route
import Template exposing (palette)



---- USER ----


type alias Flag =
    { user : User }


type alias Username =
    String


type User
    = LoggedIn Username
    | Guest


getUsername : User -> Username
getUsername user =
    case user of
        LoggedIn username ->
            username

        Guest ->
            "Guest"


toFlag : Value -> Flag
toFlag value =
    let
        cache =
            Result.andThen
                (Decode.decodeString (Decode.field "cache" string))
                (Decode.decodeValue string value)
    in
    { user = toUser (usernameDecoder cache)
    }


usernameDecoder : Result Error String -> Result Error String
usernameDecoder value =
    Result.andThen
        (Decode.decodeString (Decode.field "username" string))
        value


toUser : Result Error String -> User
toUser resultString =
    case resultString of
        Err _ ->
            Guest

        Ok username ->
            LoggedIn username



---- PAGE ----


type Page
    = Home Home.Model
    | Login Login.Model
    | Post Post.Model
    | Editor Editor.Model
    | Register Register.Model
    | Redirect
    | NotFound



---- VIEW ----


view : User -> (msg -> mainMsg) -> { title : String, content : List (Element msg) } -> Browser.Document mainMsg
view user msg { title, content } =
    { title = "Blog " ++ title
    , body =
        [ layoutWith
            Template.options
            []
          <|
            column
                [ width fill
                , height fill
                , Background.color palette.dark2
                , spacing 30
                ]
                [ viewHeader user title
                , viewContent msg content
                , viewFooter
                ]
        ]
    }


viewHeader : User -> String -> Element msg
viewHeader user title =
    row
        [ Font.color palette.purple
        , Font.size 80
        , Font.family [ Font.typeface "Baloo" ]
        , Background.color palette.dark
        , paddingXY 20 0
        , spacing 30
        , width fill
        ]
        (viewNav user title)


viewNav : User -> String -> List (Element msg)
viewNav user title =
    let
        nav name rs =
            el [ alignLeft, Font.color palette.cyan ] (text name)
                :: Template.title title
                :: List.map
                    (Route.toUrl >> link [ alignRight ])
                    rs
    in
    case user of
        LoggedIn username ->
            nav username [ Route.Home, Route.NewPost, Route.Logout ]

        Guest ->
            nav "Guest" [ Route.Home, Route.Login, Route.Register ]


viewContent : (msg -> msg1) -> List (Element msg) -> Element msg1
viewContent msg content =
    column
        [ Font.color palette.light
        , Font.size 40
        , Font.family []
        , centerX
        , Template.contentWidth
        ]
        (List.map (map msg) content)


viewFooter : Element msg
viewFooter =
    row
        [ Background.color palette.dark
        , width fill
        , padding 20
        , height (px 60)
        , alignBottom
        , Font.color palette.light
        , Font.size 30
        ]
        [ el
            [ centerX ]
            (text "This is a secret message, don't look at it!")
        ]
