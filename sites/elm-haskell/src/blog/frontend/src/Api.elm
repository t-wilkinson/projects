port module Api exposing (blog, createdBlog, get, getBlog, getBlogs, loggedIn, login, logout, onCacheChange, postBlog, put, register)

import Generated.API as Api
import Http exposing (Body, Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- a Msg Value constructor which has as a parameter a Result


type alias Response msg msg1 =
    (Result Error msg1 -> msg) -> Cmd msg



---- PORTS ----


port cache : Value -> Cmd msg


port onCacheChange : (Value -> msg) -> Sub msg


port blog : Value -> Cmd msg


port createdBlog : (() -> msg) -> Sub msg



---- API ----


login =
    Api.postLogin


register =
    Api.postRegister


loggedIn : Value -> Cmd msg
loggedIn =
    cache


logout : Cmd msg
logout =
    cache Encode.null


submitPost : Value -> Response msg ()
submitPost =
    put "post"


getBlogs =
    Api.getBlog


getBlog =
    Api.getBlogByTitle


postBlog =
    Api.postBlog



---- REQUESTS ----


get : String -> Decoder a -> Response msg a
get url decoder msg =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


post : String -> Value -> Response msg ()
post url body msg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


put : String -> Value -> Response msg ()
put url body msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> Value -> Response msg ()
delete url body msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }
