module Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Beat  =
   { beatName: String
   , beatIcon: String
   }

jsonDecBeat : Json.Decode.Decoder ( Beat )
jsonDecBeat =
   Json.Decode.succeed (\pbeatName pbeatIcon -> {beatName = pbeatName, beatIcon = pbeatIcon})
   |> required "beatName" (Json.Decode.string)
   |> required "beatIcon" (Json.Decode.string)

jsonEncBeat : Beat -> Value
jsonEncBeat  val =
   Json.Encode.object
   [ ("beatName", Json.Encode.string val.beatName)
   , ("beatIcon", Json.Encode.string val.beatIcon)
   ]



type alias Tag  =
   { tagName: String
   }

jsonDecTag : Json.Decode.Decoder ( Tag )
jsonDecTag =
   Json.Decode.succeed (\ptagName -> {tagName = ptagName}) |> custom (Json.Decode.string)

jsonEncTag : Tag -> Value
jsonEncTag  val =
   Json.Encode.string val.tagName


type alias Content  =
   { contentHeading: String
   , contentBody: String
   }

jsonDecContent : Json.Decode.Decoder ( Content )
jsonDecContent =
   Json.Decode.succeed (\pcontentHeading pcontentBody -> {contentHeading = pcontentHeading, contentBody = pcontentBody})
   |> required "contentHeading" (Json.Decode.string)
   |> required "contentBody" (Json.Decode.string)

jsonEncContent : Content -> Value
jsonEncContent  val =
   Json.Encode.object
   [ ("contentHeading", Json.Encode.string val.contentHeading)
   , ("contentBody", Json.Encode.string val.contentBody)
   ]



type alias User  =
   { userUsername: String
   , userPassword: String
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\puserUsername puserPassword -> {userUsername = puserUsername, userPassword = puserPassword})
   |> required "userUsername" (Json.Decode.string)
   |> required "userPassword" (Json.Decode.string)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("userUsername", Json.Encode.string val.userUsername)
   , ("userPassword", Json.Encode.string val.userPassword)
   ]



type alias Login  =
   { loginUsername: String
   , loginPassword: String
   }

jsonDecLogin : Json.Decode.Decoder ( Login )
jsonDecLogin =
   Json.Decode.succeed (\ploginUsername ploginPassword -> {loginUsername = ploginUsername, loginPassword = ploginPassword})
   |> required "loginUsername" (Json.Decode.string)
   |> required "loginPassword" (Json.Decode.string)

jsonEncLogin : Login -> Value
jsonEncLogin  val =
   Json.Encode.object
   [ ("loginUsername", Json.Encode.string val.loginUsername)
   , ("loginPassword", Json.Encode.string val.loginPassword)
   ]



type alias Register  =
   { registerUsername: String
   , registerPassword: String
   }

jsonDecRegister : Json.Decode.Decoder ( Register )
jsonDecRegister =
   Json.Decode.succeed (\pregisterUsername pregisterPassword -> {registerUsername = pregisterUsername, registerPassword = pregisterPassword})
   |> required "registerUsername" (Json.Decode.string)
   |> required "registerPassword" (Json.Decode.string)

jsonEncRegister : Register -> Value
jsonEncRegister  val =
   Json.Encode.object
   [ ("registerUsername", Json.Encode.string val.registerUsername)
   , ("registerPassword", Json.Encode.string val.registerPassword)
   ]



type alias StudioRequest  =
   { date: String
   , startTime: Int
   , endTime: Int
   }

jsonDecStudioRequest : Json.Decode.Decoder ( StudioRequest )
jsonDecStudioRequest =
   Json.Decode.succeed (\pdate pstartTime pendTime -> {date = pdate, startTime = pstartTime, endTime = pendTime})
   |> required "date" (Json.Decode.string)
   |> required "startTime" (Json.Decode.int)
   |> required "endTime" (Json.Decode.int)

jsonEncStudioRequest : StudioRequest -> Value
jsonEncStudioRequest  val =
   Json.Encode.object
   [ ("date", Json.Encode.string val.date)
   , ("startTime", Json.Encode.int val.startTime)
   , ("endTime", Json.Encode.int val.endTime)
   ]


postUserLogin : Login -> (Result Http.Error  (String)  -> msg) -> Cmd msg
postUserLogin body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://treywilkinson.xyz:8080"
                    [ "user"
                    , "login"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncLogin body)
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postUserRegister : Register -> (Result Http.Error  (String)  -> msg) -> Cmd msg
postUserRegister body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://treywilkinson.xyz:8080"
                    [ "user"
                    , "register"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncRegister body)
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getUserLogoutByToken : String -> (Result Http.Error  (())  -> msg) -> Cmd msg
getUserLogoutByToken capture_token toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://treywilkinson.xyz:8080"
                    [ "user"
                    , "logout"
                    , capture_token
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postStudio : StudioRequest -> (Result Http.Error  (())  -> msg) -> Cmd msg
postStudio body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "http://treywilkinson.xyz:8080"
                    [ "studio"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncStudioRequest body)
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err e)
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
