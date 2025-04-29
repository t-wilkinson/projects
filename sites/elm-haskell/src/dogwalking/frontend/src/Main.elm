module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav
import Element exposing (..)
import Page
import Page.About as About
import Page.Home as Home
import Route exposing (Route(..))
import Session exposing (Session)
import Url



---- Model ----


type Model
    = Home Home.Model
    | NotFound Session


init : flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    fromRoute (Session key) url



---- Update ----


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url.Url
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( OnUrlRequest urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl (Session.toKey (toSession model)) (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        ( OnUrlChange url, _ ) ->
            fromRoute (toSession model) url

        ( HomeMsg subMsg, Home subModel ) ->
            routeToUpdate ( Home, HomeMsg ) (Home.update subMsg subModel)

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        Home home ->
            .session home

        NotFound session ->
            session


fromRoute : Session -> Url.Url -> ( Model, Cmd Msg )
fromRoute session url =
    case Route.fromUrl url of
        Route.Home ->
            routeToUpdate ( Home, HomeMsg ) (Home.init session)

        Route.NotFound ->
            ( NotFound session, Cmd.none )


routeToUpdate : ( model -> Model, msg -> Msg ) -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
routeToUpdate ( toModel, toMsg ) ( model, msg ) =
    ( toModel model, Cmd.map toMsg msg )



---- View ----


view : Model -> Document Msg
view model =
    case model of
        Home home ->
            Page.view HomeMsg <| Home.view home

        NotFound _ ->
            { title = "NotFound", body = [ layout [] (text "NotFound") ] }



---- Program ----


main : Program () Model Msg
main =
    application
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
