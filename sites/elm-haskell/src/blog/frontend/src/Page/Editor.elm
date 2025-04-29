module Page.Editor exposing (Model, Msg, initEdit, initNew, update, view)

import Api
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Generated.API exposing (Blog, Comment)
import Http exposing (Error)
import Json.Encode as Encode
import Route
import Template exposing (palette)



---- MODEL ----


type alias Model =
    { post : Post
    , form : Form
    , username : String
    , comments : List Comment
    }


type Post
    = NewPost
    | EditPost


type alias Form =
    { title : String
    , body : String
    }


initNew : String -> ( Model, Cmd Msg )
initNew username =
    ( { post = NewPost
      , form = Form "" ""
      , username = username
      , comments = []
      }
    , Cmd.none
    )



-- when user edits, post is reposted
-- delete first post


initEdit : String -> String -> ( Model, Cmd Msg )
initEdit username title =
    ( { post = EditPost
      , form = Form "" ""
      , username = username
      , comments = []
      }
    , Api.getBlog title GotBlog
    )



---- UPDATE ----


type Msg
    = ChangedTitle String
    | ChangedBody String
    | Submitted
    | SubmitSuccess (Result Error String)
    | GotBlog (Result Error (Maybe Blog))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedTitle title ->
            updateForm model (\form -> { form | title = title })

        ChangedBody body ->
            updateForm model (\form -> { form | body = body })

        Submitted ->
            ( model
            , Api.postBlog
                { user = model.username
                , title = model.form.title
                , content = model.form.body
                , comments = model.comments
                }
                SubmitSuccess
            )

        SubmitSuccess _ ->
            ( model, Api.blog Encode.null )

        GotBlog result ->
            case result of
                Ok maybeBlog ->
                    case maybeBlog of
                        Just blog ->
                            ( { model
                                | form =
                                    { title = blog.title
                                    , body = blog.content
                                    }
                                , comments = blog.comments
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Err err ->
                    ( model, Cmd.none )


updateForm : Model -> (Form -> Form) -> ( Model, Cmd Msg )
updateForm model transform =
    ( { model | form = transform model.form }, Cmd.none )



---- VIEW ----


view : Model -> { title : String, content : List (Element Msg) }
view model =
    { title = "Editor"
    , content =
        [ viewForm model.post model.form
        ]
    }


viewForm : Post -> Form -> Element Msg
viewForm post form =
    column
        Template.form
    <|
        case post of
            NewPost ->
                viewNewPost form

            EditPost ->
                viewEditPost form


viewNewPost : Form -> List (Element Msg)
viewNewPost form =
    [ Template.formText ChangedTitle form.title Nothing "Title"
    , Template.formMultiline ChangedBody form.body Nothing "Body" True
    , row [ width fill ]
        [ Template.formSubmit
            { onPress = Just Submitted
            , label = text "Post"
            }
        , link
            [ Template.border
            , alignRight
            , padding 10
            , Background.color palette.warning
            ]
            { url = Route.home
            , label = text "Cancel"
            }
        ]
    ]


viewEditPost : Form -> List (Element Msg)
viewEditPost form =
    [ Template.formText ChangedTitle (String.slice 0 -2 form.title) Nothing "Title"
    , Template.formMultiline ChangedBody form.body Nothing "Body" True
    , row [ width fill ]
        [ Template.formSubmit
            { onPress = Just Submitted
            , label = text "Save"
            }
        , link
            [ Template.border
            , alignRight
            , padding 10
            , Background.color palette.warning
            ]
            { url = Route.home
            , label = text "Cancel"
            }
        ]
    ]
