module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Generated.API as API exposing (Blog, Comment)
import Html.Attributes as Attributes
import Http exposing (Error)
import Markdown
import Route
import Template exposing (palette)



---- MODEL ----


type alias Model =
    { posts : List Blog }


init : ( Model, Cmd Msg )
init =
    ( { posts = [] }, Api.getBlogs GotPosts )



---- UPDATE ----


type Msg
    = GotPosts (Result Error (List Blog))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPosts result ->
            case result of
                Ok posts ->
                    ( { model | posts = posts }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> { title : String, content : List (Element Msg) }
view model =
    { title = "Home"
    , content =
        [ column
            [ spacing 30
            , paddingEach { top = 0, right = 0, bottom = 80, left = 0 }
            ]
            (List.map linkPost model.posts)
        ]
    }


linkPost : Blog -> Element Msg
linkPost blog =
    let
        route =
            Route.toUrl (Route.Post blog.title)
    in
    link []
        { route | label = blogPost blog }


blogPost : Blog -> Element Msg
blogPost { user, title, content } =
    column
        [ Background.color palette.dark
        , padding 40
        , spacing 20
        , width (px 1000)
        , Template.border
        ]
        [ paragraph []
            [ el [ Font.color palette.cyan, Font.size 60 ] (text user)
            , el
                [ Font.size 30, Font.color palette.gray2 ]
                (text (" - " ++ String.slice 0 -2 title))
            ]
        , Template.bar
        , paragraph
            []
            [ el [] (text (String.left 40 content))
            , el [ Font.color palette.gray ] (text " .....")
            ]
        ]
