module Session exposing (Session, fromFlags, toKey, updateDevice)

import Browser.Navigation as Nav
import Element exposing (Device, classifyDevice)
import Json.Decode as Decode exposing (Decoder, Value, field, int, map2, string)
import Styles exposing (Height, Width)


type alias Session =
    { nav : Nav.Key
    , device : Device
    }


toKey : Session -> Nav.Key
toKey session =
    session.nav


fromFlags : Value -> { device : Device }
fromFlags flags =
    let
        device =
            Result.withDefault { width = 0, height = 0 } <|
                Decode.decodeValue (Decode.field "device" deviceDecoder) flags
    in
    { device = classifyDevice device }



---- Device ----


type alias DecodeDevice =
    { width : Width, height : Height }


updateDevice : Session -> Width -> Height -> Session
updateDevice session width height =
    { session
        | device =
            classifyDevice { width = width, height = height }
    }


deviceDecoder : Decoder DecodeDevice
deviceDecoder =
    map2 DecodeDevice
        (field "width" int)
        (field "height" int)
