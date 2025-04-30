module Page.About exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (..)



--- Model ---


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )



--- Update ---


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



--- View ---


type alias Section =
    Model -> Html Msg


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "About"
    , content =
        main_
            [ id "about" ]
            [ h2 [] [ text "about me" ] ]
    }
