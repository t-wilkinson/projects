module Templates exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Json
import Session


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)


viewError : Error -> String
viewError error =
    case error of
        BadUrl url ->
            "Bad url: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network Error"

        BadStatus status ->
            "Bad Status: " ++ String.fromInt status

        BadBody body ->
            "Bad body: " ++ body



--- Input ---


viewInput :
    ((form -> String -> form) -> String -> msg)
    -> List (Session.Error formField)
    -> String
    -> formField
    -> (form -> String -> form)
    -> String
    -> String
    -> Html msg
viewInput msg error field formField updateForm placeholder type__ =
    section [ class "field" ]
        [ input
            [ onInput (msg updateForm)
            , value field
            , type_ type__
            , id placeholder
            , required True
            ]
            []
        , span [] []
        , label
            [ for placeholder ]
            [ text placeholder ]
        , viewFormErrors formField error
        ]


viewFormErrors : formField -> List (Session.Error formField) -> Html msg
viewFormErrors field errors =
    errors
        |> List.filter (\( fieldError, _ ) -> fieldError == field)
        |> List.map (\( _, error ) -> li [] [ text error ])
        |> ul [ class "form-error" ]
