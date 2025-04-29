module Generated.API exposing(..)

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

type alias Blog  =
   { user: String
   , title: String
   , content: String
   , comments: (List Comment)
   }

jsonDecBlog : Json.Decode.Decoder ( Blog )
jsonDecBlog =
   Json.Decode.succeed (\puser ptitle pcontent pcomments -> {user = puser, title = ptitle, content = pcontent, comments = pcomments})
   |> required "user" (Json.Decode.string)
   |> required "title" (Json.Decode.string)
   |> required "content" (Json.Decode.string)
   |> required "comments" (Json.Decode.list (jsonDecComment))

jsonEncBlog : Blog -> Value
jsonEncBlog  val =
   Json.Encode.object
   [ ("user", Json.Encode.string val.user)
   , ("title", Json.Encode.string val.title)
   , ("content", Json.Encode.string val.content)
   , ("comments", (Json.Encode.list jsonEncComment) val.comments)
   ]



type alias User  =
   { username: String
   , password: String
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pusername ppassword -> {username = pusername, password = ppassword})
   |> required "username" (Json.Decode.string)
   |> required "password" (Json.Decode.string)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("username", Json.Encode.string val.username)
   , ("password", Json.Encode.string val.password)
   ]



type alias Comment  =
   { commentUser: String
   , commentContent: String
   }

jsonDecComment : Json.Decode.Decoder ( Comment )
jsonDecComment =
   Json.Decode.succeed (\pcommentUser pcommentContent -> {commentUser = pcommentUser, commentContent = pcommentContent})
   |> required "commentUser" (Json.Decode.string)
   |> required "commentContent" (Json.Decode.string)

jsonEncComment : Comment -> Value
jsonEncComment  val =
   Json.Encode.object
   [ ("commentUser", Json.Encode.string val.commentUser)
   , ("commentContent", Json.Encode.string val.commentContent)
   ]


getUser : (Result Http.Error  ((List User))  -> msg) -> Cmd msg
getUser toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "user"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecUser))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteUser : (Result Http.Error  (())  -> msg) -> Cmd msg
deleteUser toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "user"
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

postLogin : User -> (Result Http.Error  (String)  -> msg) -> Cmd msg
postLogin body toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "login"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUser body)
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postRegister : User -> (Result Http.Error  (String)  -> msg) -> Cmd msg
postRegister body toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "register"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUser body)
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getBlog : (Result Http.Error  ((List Blog))  -> msg) -> Cmd msg
getBlog toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "blog"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecBlog))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteBlog : (Result Http.Error  (())  -> msg) -> Cmd msg
deleteBlog toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "blog"
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

deleteBlogByTitle : String -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteBlogByTitle capture_title toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "blog"
                    , capture_title
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

getBlogByTitle : String -> (Result Http.Error  ((Maybe Blog))  -> msg) -> Cmd msg
getBlogByTitle capture_title toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "blog"
                    , capture_title
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (jsonDecBlog))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postBlog : Blog -> (Result Http.Error  (String)  -> msg) -> Cmd msg
postBlog body toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "blog"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncBlog body)
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postCommentByTitle : String -> Comment -> (Result Http.Error  (())  -> msg) -> Cmd msg
postCommentByTitle capture_title body toMsg =
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
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "comment"
                    , capture_title
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncComment body)
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

deleteCommentByTitle : String -> Comment -> (Result Http.Error  (())  -> msg) -> Cmd msg
deleteCommentByTitle capture_title body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "https://treywilkinson.xyz:8080"
                    [ "comment"
                    , capture_title
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncComment body)
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
