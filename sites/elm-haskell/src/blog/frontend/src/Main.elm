module Main exposing (..)

import Api
import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (src)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page exposing (Page(..), User(..))
import Page.Editor as Editor
import Page.Home as Home
import Page.Login as Login
import Page.Post as Post
import Page.Register as Register
import Route exposing (Route)
import Url exposing (Url)



---- MODEL ----


type alias Model =
    { navKey : Nav.Key
    , user : User
    , page : Page
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init value url navKey =
    let
        flags =
            Page.toFlag value
    in
    routeToPage
        (Model navKey flags.user Redirect)
        (Route.fromUrl url)



---- UPDATE ----


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | RegisterMsg Register.Msg
    | PostMsg Post.Msg
    | EditorMsg Editor.Msg
    | CacheChanged Value
    | CreatedBlog ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External string ->
                    ( model, Nav.load string )

        ( OnUrlChange url, _ ) ->
            routeToPage model <| Route.fromUrl url

        ( CacheChanged value, _ ) ->
            let
                username =
                    value
                        |> Decode.decodeValue
                            (Decode.field "username" Decode.string)
                        >> Result.toMaybe
                        >> Maybe.withDefault ""
            in
            ( { model | user = LoggedIn username }
            , Nav.pushUrl model.navKey Route.home
            )

        ( CreatedBlog _, _ ) ->
            ( model, Nav.pushUrl model.navKey Route.home )

        ( HomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> toUpdate model Home HomeMsg

        ( LoginMsg subMsg, Login subModel ) ->
            Login.update subMsg subModel
                |> toUpdate model Login LoginMsg

        ( RegisterMsg subMsg, Register subModel ) ->
            Register.update subMsg subModel
                |> toUpdate model Register RegisterMsg

        ( PostMsg subMsg, Post subModel ) ->
            Post.update subMsg subModel
                |> toUpdate model Post PostMsg

        ( EditorMsg subMsg, Editor subModel ) ->
            Editor.update subMsg subModel
                |> toUpdate model Editor EditorMsg

        _ ->
            ( model, Cmd.none )


toUpdate :
    Model
    -> (model -> Page)
    -> (msg -> Msg)
    -> ( model, Cmd msg )
    -> ( Model, Cmd Msg )
toUpdate model toPage toMsg ( subModel, subMsg ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subMsg
    )


routeToPage : Model -> Maybe Route -> ( Model, Cmd Msg )
routeToPage model maybeRoute =
    let
        username =
            Page.getUsername model.user
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Home ->
            toUpdate model Home HomeMsg Home.init

        Just (Route.Post title) ->
            toUpdate model Post PostMsg (Post.init title username)

        Just (Route.EditPost title) ->
            toUpdate model Editor EditorMsg (Editor.initEdit username title)

        Just Route.NewPost ->
            toUpdate model Editor EditorMsg (Editor.initNew username)

        Just Route.Login ->
            toUpdate model Login LoginMsg Login.init

        Just Route.Register ->
            toUpdate model Register RegisterMsg Register.init

        Just Route.Logout ->
            ( { model | user = Guest }
            , Cmd.batch [ Nav.pushUrl model.navKey Route.home, Api.logout ]
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Home home ->
            Page.view model.user HomeMsg <| Home.view home

        Login login ->
            Page.view model.user LoginMsg <| Login.view login

        Register login ->
            Page.view model.user RegisterMsg <| Register.view login

        Post post ->
            Page.view model.user PostMsg <| Post.view post

        Editor editor ->
            Page.view model.user EditorMsg <| Editor.view editor

        Redirect ->
            { title = "Redirect", body = [ layout [] (text "Redirecting") ] }

        NotFound ->
            { title = "NotFound", body = [ layout [] (text "NotFound") ] }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Api.onCacheChange CacheChanged
        , Api.createdBlog CreatedBlog
        , subSubscriptions model.page
        ]


subSubscriptions : Page -> Sub Msg
subSubscriptions page =
    case page of
        Login subModel ->
            Sub.map LoginMsg (Login.subscriptions subModel)

        _ ->
            Sub.none


toSubscription :
    Model
    -> (msg -> Msg)
    -> ( model, Sub msg )
    -> Sub Msg
toSubscription model toSub ( subModel, subMsg ) =
    Sub.map toSub subMsg



---- MAIN ----


main : Program Value Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
