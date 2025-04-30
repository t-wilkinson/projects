module Main exposing (..)

-- import Page.Attendance as Attendance

import Api exposing (Day(..), Days, People, Person)
import Browser exposing (..)
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Json.Decode exposing (Value)
import Page
import Page.Attendance as Attendance
import Page.Home as Home
import Route exposing (Route(..))
import Session exposing (Session)
import Styles exposing (Height, Width)
import Url



--- MODEL ---


type Model
    = Home Home.Model
    | Attendance Attendance.Model
    | NotFound { session : Session }


type alias Toggle =
    { person : Person
    , checked : Bool
    }


init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        { device } =
            Session.fromFlags flags
    in
    fromRoute (Session key device) (Route.fromUrl url)



--- UPDATE ---


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url.Url
    | OnResize Int Int
    | HomeMsg Home.Msg
    | AttendanceMsg Attendance.Msg


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
            fromRoute (toSession model) (Route.fromUrl url)

        ( OnResize width height, _ ) ->
            ( onResize model width height, Cmd.none )

        ( HomeMsg subMsg, Home subModel ) ->
            toUpdate (Home.update subMsg subModel) Home HomeMsg

        ( AttendanceMsg subMsg, Attendance subModel ) ->
            toUpdate (Attendance.update subMsg subModel) Attendance AttendanceMsg

        _ ->
            ( model, Cmd.none )


onResize : Model -> Width -> Height -> Model
onResize model width height =
    let
        updateDevice subModel =
            { subModel | session = Session.updateDevice subModel.session width height }
    in
    case model of
        Home subModel ->
            Home <| updateDevice subModel

        Attendance subModel ->
            Attendance <| updateDevice subModel

        NotFound subModel ->
            NotFound <| updateDevice subModel


toSession : Model -> Session
toSession model =
    case model of
        Home subModel ->
            subModel.session

        Attendance subModel ->
            subModel.session

        NotFound subModel ->
            subModel.session


fromRoute : Session -> Route -> ( Model, Cmd Msg )
fromRoute session route =
    case route of
        Route.Home ->
            toUpdate (Home.init session) Home HomeMsg

        Route.Attendance ->
            toUpdate (Attendance.init session) Attendance AttendanceMsg

        Route.NotFound ->
            ( NotFound { session = session }, Cmd.none )


toUpdate : ( model, Cmd msg ) -> (model -> Model) -> (msg -> Msg) -> ( Model, Cmd Msg )
toUpdate ( model, msg ) toModel toMsg =
    ( toModel model, Cmd.map toMsg msg )



--- VIEW ---


view : Model -> Document Msg
view model =
    case model of
        Home home ->
            Page.view HomeMsg (Home.view home)

        Attendance home ->
            Page.view AttendanceMsg (Attendance.view home)

        NotFound _ ->
            { title = "NotFound", body = [ layout [] (link [] { url = "/", label = text "notfound" }) ] }



--- Subscriptions ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize OnResize



--- PROGRAM ---


main : Program Value Model Msg
main =
    application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
