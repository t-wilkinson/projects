module Page.Home exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Session exposing (Session)
import Styles exposing (palette)



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
    { title = "Home"
    , content =
        column
            [ width fill
            ]
            [ text "This is the home page"
            , viewCalendar model
            ]
    }


viewCalendar : Model -> Element Msg
viewCalendar model =
    column
        [ width (px 400)
        , Font.color palette.white
        , Border.color palette.darkPink
        , Border.width 4
        , centerX
        ]
        [ el
            [ width fill
            , Background.color palette.darkPink
            , paddingXY 0 5
            ]
            (text "Month")
        , row
            [ Font.color palette.pink
            , width fill
            , paddingXY 20 0
            , spaceEvenly
            ]
          <|
            List.map (\dow -> el [] (text dow)) [ "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" ]
        ]
