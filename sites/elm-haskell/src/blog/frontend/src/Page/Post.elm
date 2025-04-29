module Page.Post exposing (Model, Msg, init, update, view)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Generated.API as API exposing (Blog, Comment)
import Http exposing (Error)
import Json.Encode as Encode
import Markdown
import Route
import Set
import Template exposing (palette)



---- MODEL ----


type alias Model =
    { blog : Status Blog
    , form : Form
    , user : String
    }


type Status a
    = Loading
    | Loaded a
    | Failed


type alias Form =
    { comment : String }


init : String -> String -> ( Model, Cmd Msg )
init title user =
    ( { blog = Loading
      , form = Form ""
      , user = user
      }
    , API.getBlogByTitle title GotBlog
    )



---- UPDATE ----


type Msg
    = ChangedContent String
    | SubmittedComment
    | GotBlog (Result Error (Maybe Blog))
    | AddedComment (Result Error ())
    | DeleteBlog String
    | DeletedBlog (Result Error ())
    | DeleteComment Comment
    | DeletedComment (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedContent comment ->
            updateForm model (\form -> { form | comment = comment })

        SubmittedComment ->
            case model.blog of
                Loading ->
                    ( model, Cmd.none )

                Failed ->
                    ( model, Cmd.none )

                Loaded blog ->
                    if String.length model.form.comment <= 0 then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | form = Form ""
                            , blog =
                                Loaded
                                    { blog | comments = toComment model :: blog.comments }
                          }
                        , API.postCommentByTitle blog.title (toComment model) AddedComment
                        )

        AddedComment result ->
            ( model, Cmd.none )

        GotBlog result ->
            case result of
                Ok maybeBlog ->
                    case maybeBlog of
                        Just blog ->
                            ( { model | blog = Loaded blog }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | blog = Failed }, Cmd.none )

                Err err ->
                    ( { model | blog = Failed }, Cmd.none )

        DeleteBlog path ->
            ( model, API.deleteBlogByTitle path DeletedBlog )

        DeletedBlog result ->
            ( model, Api.blog Encode.null )

        DeleteComment comment ->
            case model.blog of
                Loading ->
                    ( model, Cmd.none )

                Failed ->
                    ( model, Cmd.none )

                Loaded blog ->
                    ( { model | blog = Loaded { blog | comments = List.filter (\c -> c /= comment) blog.comments } }
                    , API.deleteCommentByTitle blog.title comment DeletedComment
                    )

        DeletedComment result ->
            ( model, Cmd.none )


updateForm : Model -> (Form -> Form) -> ( Model, Cmd Msg )
updateForm model transform =
    ( { model | form = transform model.form }, Cmd.none )


toComment : Model -> Comment
toComment model =
    { commentContent = model.form.comment
    , commentUser = model.user
    }



---- VIEW ----


view : Model -> { title : String, content : List (Element Msg) }
view model =
    { title = "Post"
    , content =
        [ column [ spacing 30 ]
            [ viewBlog model
            , viewFormComment (toComment model)
            ]
        ]
    }


viewBlog : Model -> Element Msg
viewBlog model =
    case model.blog of
        Failed ->
            text "Failed"

        Loading ->
            text "Loading..."

        Loaded blog ->
            column
                [ spacing 30
                ]
                [ viewPost blog model
                , viewComments model.user blog.comments
                ]


viewPost : Blog -> Model -> Element Msg
viewPost blog { user } =
    column
        [ Background.color palette.dark
        , Template.border
        , paddingXY 30 20
        , spacing 10
        , width (px 1000)
        ]
        [ paragraph
            []
            [ el
                [ Font.color palette.cyan, Font.size 60 ]
                (text blog.user)
            , el
                [ Font.size 30, Font.color palette.gray2 ]
                (text (" - " ++ String.slice 0 -2 blog.title))
            ]
        , el
            [ height (px 3)
            , moveDown 10
            , Background.color palette.gray
            , width fill
            ]
            none
        , paragraph
            -- does not display new lines
            [ width fill, Font.size 28, padding 20 ]
            [ html <|
                Markdown.toHtmlWith
                    { githubFlavored =
                        Just
                            { tables = True, breaks = True }
                    , defaultHighlighting = Just "elm"
                    , sanitize = blog.user /= "admin"
                    , smartypants = True
                    }
                    []
                    blog.content
            ]
        , if blog.user == user || user == "admin" then
            row [ width fill ]
                [ link
                    [ Background.color palette.green
                    , Font.color palette.dark
                    , Template.border
                    , Font.size 20
                    , padding 6
                    ]
                    (blog.title |> Route.EditPost >> Route.toUrl)
                , Template.deleteButton
                    (Just (DeleteBlog blog.title))
                    (text "Delete Post")
                ]

          else
            none
        ]


viewComments : String -> List Comment -> Element Msg
viewComments username comments =
    if List.isEmpty comments then
        none

    else
        column
            [ Background.color palette.dark
            , width fill
            , Template.border
            ]
            [ el Template.labels (text "Comments")
            , column [ padding 10 ] <|
                List.intersperse
                    Template.bar
                    (List.map (viewComment username) comments)
            ]


viewComment : String -> Comment -> Element Msg
viewComment username comment =
    row
        [ width (px 980)
        , paddingXY 10 20
        , Font.color palette.light
        , spacing 10
        ]
        [ paragraph [ spacing 0, width fill ]
            [ el
                [ padding 20, Font.color palette.green ]
                (text (comment.commentUser ++ ": "))
            , el
                [ Font.color palette.light ]
                (text comment.commentContent)
            ]
        , if
            comment.commentUser
                == username
                || username
                == "admin"
          then
            Template.deleteButton
                (Just (DeleteComment comment))
                (text "x")

          else
            none
        ]


viewFormComment : Comment -> Element Msg
viewFormComment comment =
    column
        Template.form
        [ Template.formSubmitOnEnter
            SubmittedComment
            ChangedContent
            comment.commentContent
            Nothing
            "Post Comment"
        ]
