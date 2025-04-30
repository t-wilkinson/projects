module Api exposing (..)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Url.Builder


maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
    case mx of
        Nothing ->
            ""

        Just True ->
            "1"

        Just False ->
            "0"


type alias People =
    List Person


jsonDecPeople : Json.Decode.Decoder People
jsonDecPeople =
    Json.Decode.list jsonDecPerson


jsonEncPeople : People -> Value
jsonEncPeople val =
    Json.Encode.list jsonEncPerson val


type alias Days =
    List Day


jsonDecDays : Json.Decode.Decoder Days
jsonDecDays =
    Json.Decode.list jsonDecDay


jsonEncDays : Days -> Value
jsonEncDays val =
    Json.Encode.list jsonEncDay val


type alias Person =
    { name : String
    , icon : String
    }


jsonDecPerson : Json.Decode.Decoder Person
jsonDecPerson =
    Json.Decode.succeed (\pname picon -> { name = pname, icon = picon })
        |> required "name" Json.Decode.string
        |> required "icon" Json.Decode.string


jsonEncPerson : Person -> Value
jsonEncPerson val =
    Json.Encode.object
        [ ( "name", Json.Encode.string val.name )
        , ( "icon", Json.Encode.string val.icon )
        ]


type Day
    = Day ( Int, Int, Int ) (List Person)


jsonDecDay : Json.Decode.Decoder Day
jsonDecDay =
    Json.Decode.lazy (\_ -> Json.Decode.map2 Day (Json.Decode.index 0 (Json.Decode.map3 tuple3 (Json.Decode.index 0 Json.Decode.int) (Json.Decode.index 1 Json.Decode.int) (Json.Decode.index 2 Json.Decode.int))) (Json.Decode.index 1 (Json.Decode.list jsonDecPerson)))


jsonEncDay : Day -> Value
jsonEncDay (Day v1 v2) =
    Json.Encode.list identity [ (\( t1, t2, t3 ) -> Json.Encode.list identity [ Json.Encode.int t1, Json.Encode.int t2, Json.Encode.int t3 ]) v1, Json.Encode.list jsonEncPerson v2 ]


getPeople : (Result Http.Error People -> msg) -> Cmd msg
getPeople toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "people"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg jsonDecPeople
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postPeople : List Person -> (Result Http.Error () -> msg) -> Cmd msg
postPeople body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "people"
                ]
                params
        , body =
            Http.jsonBody (Json.Encode.list jsonEncPerson body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getDays : (Result Http.Error Days -> msg) -> Cmd msg
getDays toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8000"
                [ "days"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg jsonDecDays
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
