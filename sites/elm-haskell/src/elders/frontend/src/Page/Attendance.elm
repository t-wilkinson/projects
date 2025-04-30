module Page.Attendance exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Font as Font
import Session exposing (Session)
import Styles exposing (palette)



---- Model ----


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



---- Update ----


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- View ----


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Attendance"
    , content =
        column
            []
            []
    }
