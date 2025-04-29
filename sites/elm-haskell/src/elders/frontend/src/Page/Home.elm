module Page.Home exposing (Model, Msg, init, update, view)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http exposing (Error)
import Session exposing (Session)
import Styles exposing (palette)



---- Model ----


type alias Model =
    { session : Session
    , days : Api.Days
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , days = []
      }
    , Api.getDays GotDays
    )



---- Update ----


type Msg
    = GotDays (Result Error Api.Days)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDays result ->
            case result of
                Ok days ->
                    ( { model | days = days }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )



---- View ----


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Home"
    , content =
        column
            []
            [ wrappedRow
                [ spacing 30
                , paddingXY 5 20
                , width fill
                ]
                (List.map viewDay model.days)
            ]
    }


viewDay : Api.Day -> Element Msg
viewDay (Api.Day ( year, month, day ) people) =
    column
        [ Font.alignLeft
        , width (minimum 300 fill |> maximum 400)
        ]
        [ el
            [ Font.color palette.white
            , Background.color palette.blue
            , Font.size 20
            , Font.extraBold
            , width fill
            , paddingXY 10 5
            , Border.roundEach
                { topLeft = 6
                , topRight = 6
                , bottomLeft = 0
                , bottomRight = 0
                }
            ]
            ([ year, month, day ]
                |> List.map String.fromInt
                >> String.join "/"
                >> text
            )

        -- display a list of people for each day
        , people
            |> List.map .name
            >> String.join "\n"
            >> text
            >> el
                [ width fill
                , centerX
                , height (px 200)

                --, height (maximum 200 fill)
                , clipY
                , scrollbarY
                , padding 10
                , Font.size 30
                , Font.color palette.black
                , Border.color palette.blue
                , Border.width 2
                , Border.roundEach
                    { topLeft = 0
                    , topRight = 0
                    , bottomLeft = 6
                    , bottomRight = 6
                    }
                ]
        ]
