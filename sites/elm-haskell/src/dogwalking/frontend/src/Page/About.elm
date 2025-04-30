module Page.About exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Session exposing (Session)



---- Model ----


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
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
    { title = "About"
    , content =
        column [] [ text "This is the home page" ]
    }
