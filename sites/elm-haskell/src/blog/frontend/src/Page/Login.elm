module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Animation exposing (State)
import Api
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http exposing (Error)
import Json.Encode as Encode exposing (Value)
import Template exposing (palette)



---- MODEL ----


type alias Model =
    { form : Form
    , style : Animation.State
    , problems : List Problem
    , status : Bool
    }


type alias Form =
    { username : String
    , password : String
    }


type Problem
    = InvalidField Field String
    | ServerError String


init : ( Model, Cmd msg )
init =
    ( { form = Form "" ""
      , style =
            Animation.style
                [ Animation.left (Animation.px 0.0)
                , Animation.opacity 1.0
                ]
      , problems = []
      , status = False
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | Submitted
    | SubmitSuccess (Result Error String)
    | Animate Animation.Msg
    | Show


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUsername newUsername ->
            updateForm model (\form -> { form | username = newUsername })

        ChangedPassword newPassword ->
            updateForm model (\form -> { form | password = newPassword })

        Submitted ->
            case validate model.form of
                Ok form ->
                    ( model
                    , Api.login model.form SubmitSuccess
                    )

                Err problems ->
                    ( { model | problems = problems }, Cmd.none )

        SubmitSuccess result ->
            -- server will validate input and return responce
            -- if success, send a cache change
            -- else, show errors
            case result of
                Ok "success" ->
                    ( model
                    , Api.loggedIn <|
                        Encode.object
                            [ ( "username"
                              , Encode.string model.form.username
                              )
                            ]
                    )

                Ok error ->
                    ( { model | problems = [ ServerError error ] }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Animate animate ->
            ( { model | style = Animation.update animate model.style }
            , Cmd.none
            )

        Show ->
            ( { model
                | style =
                    if not model.status then
                        to model.style

                    else
                        from model.style
                , status = not model.status
              }
            , Cmd.none
            )


from =
    Template.interrupt [ Animation.to [ Animation.opacity 1.0 ] ]


to =
    Template.interrupt [ Animation.to [ Animation.opacity 0.0 ] ]


updateForm : Model -> (Form -> Form) -> ( Model, Cmd Msg )
updateForm model toForm =
    ( { model | form = toForm model.form }, Cmd.none )


encodeForm : Form -> Value
encodeForm form =
    Encode.object
        [ ( "username", Encode.string form.username )
        , ( "password", Encode.string form.password )
        ]



---- VALIDATION ----


type Field
    = Username
    | Password


validate : Form -> Result (List Problem) Form
validate form =
    let
        trimmedForm =
            { username = String.trim form.username
            , password = String.trim form.password
            }
    in
    case List.concatMap (validateField trimmedForm) [ Username, Password ] of
        [] ->
            Ok form

        err ->
            Err err


validateField : Form -> Field -> List Problem
validateField form field =
    List.map (InvalidField field) <|
        case field of
            Username ->
                []

            Password ->
                []



---- VIEW ----


view : Model -> { title : String, content : List (Element Msg) }
view model =
    { title = "Login"
    , content =
        [ column [] (List.map viewProblem model.problems)
        , viewForm model
        , Input.button
            [ Template.border ]
            { onPress = Just Show
            , label =
                el
                    [ Background.color palette.green
                    , Font.color palette.dark4
                    , Template.border
                    ]
                    (text "Magic Trick")
            }
        ]
    }


viewProblem : Problem -> Element Msg
viewProblem problem =
    case problem of
        InvalidField _ txt ->
            text txt

        ServerError txt ->
            text txt


viewForm : Model -> Element Msg
viewForm { form, style } =
    column
        (Template.form ++ Template.render style)
        [ Template.username ChangedUsername form.username Nothing
        , Template.currentPassword ChangedPassword form.password Nothing
        , Template.formSubmit
            { onPress = Just Submitted
            , label = text "Submit"
            }
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.style ]
