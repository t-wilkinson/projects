module Page.Studio exposing (Model, Msg, init, update, view)

import Api
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Http
import Json.Decode as Decode
import Session exposing (..)
import Templates



--- Model ---


type alias Model =
    { session : Session
    , datePicker : DatePicker.DatePicker
    , form : Form
    }


type alias Form =
    { date : Maybe Date
    , startTime : Maybe Int
    , endTime : Maybe Int
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        form =
            { date = Nothing
            , startTime = Nothing
            , endTime = Nothing
            }
    in
    ( { session = session
      , datePicker = datePicker
      , form = form
      }
    , Cmd.map SetDatePicker datePickerCmd
    )


settings : DatePicker.Settings
settings =
    { defaultSettings
        | inputClassList = [ ( "form-control", True ) ]
        , inputId = Just "datepicker"
    }



--- Update ---


type Msg
    = Changed (Form -> String -> Form) String
    | SetDatePicker DatePicker.Msg
    | SetStartTime Int
    | SetEndTime Int
    | SubmitRequest
    | SubmittedRequest (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Changed f s ->
            ( model, Cmd.none )

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update settings subMsg model.datePicker

                date =
                    case dateEvent of
                        Picked newDate ->
                            Just newDate

                        _ ->
                            model.form.date
            in
            ( { model
                | form = (\f s -> { f | date = s }) model.form date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )

        SetStartTime n ->
            ( { model | form = (\f x -> { f | startTime = Just x }) model.form n }
            , Cmd.none
            )

        SetEndTime n ->
            ( { model | form = (\f x -> { f | endTime = Just x }) model.form n }
            , Cmd.none
            )

        SubmitRequest ->
            let
                { date, startTime, endTime } =
                    model.form
            in
            case ( date, startTime, endTime ) of
                ( Just d, Just st, Just et ) ->
                    ( model
                    , Api.postStudio
                        (Api.StudioRequest "" st et)
                        SubmittedRequest
                    )

                _ ->
                    ( model, Cmd.none )

        SubmittedRequest (Ok ()) ->
            Debug.log "ok" <| ( model, Cmd.none )

        SubmittedRequest (Err error) ->
            Debug.log "err" <| ( model, Cmd.none )



--- View ---


type alias Section =
    Model -> Html Msg


view : Model -> { title : String, content : Html Msg }
view model =
    { title = ""
    , content =
        main_
            [ id "studio" ]
            [ viewRequest model
            , viewPrices model
            , viewLocation model
            , viewEquipment model
            , viewAmenities model
            , viewOccupancy model
            ]
    }



-- Prices


viewPrices : Section
viewPrices model =
    section
        [ id "prices" ]
        [ h2 [] [ text "Prices" ]
        , ul []
            [ viewPrice "Recording" "$30"
            , viewPrice "Mixing" "$40"
            , viewPrice "Mastering" "$15"
            ]
        ]


viewPrice : String -> String -> Html Msg
viewPrice heading price =
    li [ class "js-tilt" ]
        [ h3 [] [ text heading ]
        , span [] [ text price ]
        ]



-- Request


viewRequest : Section
viewRequest model =
    section
        [ id "request" ]
        [ h2 [] [ text "Schedule studio time" ]
        , viewForm model
        ]


viewForm : Section
viewForm model =
    Html.form
        [ onSubmit SubmitRequest ]
        [ viewClock model
        , viewStartTime model
        , viewEndTime model
        , button [] [ text "Submit" ]
        ]


viewClock : Section
viewClock { datePicker, form } =
    article
        [ id "clock" ]
        [ case form.date of
            Nothing ->
                div [] []

            Just date ->
                h1 [] [ text <| Date.format "MMM d, yyyy" date ]
        , DatePicker.view form.date settings datePicker
            |> Html.map SetDatePicker
        ]


viewOption : String -> Int -> List (Html Msg)
viewOption period time =
    let
        n =
            String.fromInt time
    in
    [ option [ value n ] [ text (n ++ ":00" ++ period) ]
    , option [ value n ] [ text (n ++ ":30" ++ period) ]
    ]


viewStartTime : Section
viewStartTime { form } =
    select
        [ on "change" (Decode.map SetStartTime targetValueIntParse) ]
    <|
        [ option [ value "0000" ] [ text "00:00 am" ] ]
            ++ [ option [ value "1200" ] [ text "12:00 am" ]
               , option [ value "1230" ] [ text "12:30 am" ]
               ]
            ++ List.concatMap (viewOption " am") (List.range 1 11)
            ++ [ option [ value "1200" ] [ text "12:00 pm" ]
               , option [ value "1230" ] [ text "12:30 pm" ]
               ]
            ++ List.concatMap (viewOption " pm") (List.range 1 11)


viewEndTime : Section
viewEndTime { form } =
    select
        [ on "change" (Decode.map SetEndTime targetValueIntParse) ]
    <|
        [ option [ value "0000" ] [ text "00:00 am" ] ]
            ++ [ option [ value "1200" ] [ text "12:00 am" ]
               , option [ value "1230" ] [ text "12:30 am" ]
               ]
            ++ List.concatMap (viewOption " am") (List.range 1 11)
            ++ [ option [ value "1200" ] [ text "12:00 pm" ]
               , option [ value "1230" ] [ text "12:30 pm" ]
               ]
            ++ List.concatMap (viewOption " pm") (List.range 1 11)



-- Social Medias


viewSocialMedias : Section
viewSocialMedias model =
    section [ id "social-medias" ]
        [ h2 [] [ text "Check out my social media" ]
        , ul []
            -- Soundcloud
            [ viewSocialMedia "Soundcloud"
                "@micheal_klean"
                "https://soundcloud.com/michael_klean/tracks"

            -- Youtube
            , viewSocialMedia "Youtube"
                "@Max K"
                "https://www.youtube.com/channel/UCYiQt_7i1a-wz-533kTYNZQ?view_as=subscriber"

            -- Twitter
            , viewSocialMedia "Twitter"
                "@mtklim20"
                "https://twitter.com/mtklim20"

            -- Instagram
            , viewSocialMedia "Instagram"
                "@michael_klean"
                ""
            ]
        ]


viewSocialMedia : String -> String -> String -> Html Msg
viewSocialMedia platform text_ ref =
    li []
        [ text platform
        , a
            [ href ref
            , target "_blank"
            ]
            [ text text_ ]
        ]



-- Location


viewLocation : Section
viewLocation model =
    section
        []
        [ h2 [] [ text "Location" ]
        ]



-- Equipment


viewEquipment : Section
viewEquipment model =
    section
        []
        [ h2 [] [ text "Equipment" ]
        ]



-- Amenities


viewAmenities : Section
viewAmenities model =
    section
        []
        [ h2 [] [ text "Amenities" ]
        ]



-- Occupancy


viewOccupancy : Section
viewOccupancy model =
    section
        []
        [ h2 [] [ text "Occupancy" ]
        ]
