module Page.Register exposing (Model, Msg, init, update, view)

import Api
import Element exposing (..)
import Element.Input as Input
import Http exposing (Error)
import Json.Encode as Encode
import Template



---- MODEL ----


type alias Model =
    { form : Form
    , problems : List Problem
    }


type alias Form =
    { username : String
    , password : String
    , email : String
    }


type Problem
    = InvalidField Field String
    | ServerError String


init : ( Model, Cmd Msg )
init =
    ( { form = Form "" "" ""
      , problems = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangedUsername String
    | ChangedPassword String
    | ChangedEmail String
    | SubmitSuccess (Result Error String)
    | Submitted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUsername username ->
            updateForm model (\form -> { form | username = username })

        ChangedPassword password ->
            updateForm model (\form -> { form | password = password })

        ChangedEmail email ->
            updateForm model (\form -> { form | email = email })

        Submitted ->
            case validate model.form of
                Ok form ->
                    ( model
                    , Api.register
                        { username = model.form.username
                        , password = model.form.password
                        }
                        SubmitSuccess
                    )

                Err problems ->
                    ( { model | problems = problems }, Cmd.none )

        SubmitSuccess result ->
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


updateForm : Model -> (Form -> Form) -> ( Model, Cmd Msg )
updateForm model toForm =
    ( { model | form = toForm model.form }, Cmd.none )



---- VALIDATION ----


type Field
    = Username
    | Password
    | Email


validate : Form -> Result (List Problem) Form
validate form =
    let
        trimmedForm =
            { username = String.trim form.username
            , password = String.trim form.password
            , email = String.trim form.email
            }
    in
    case List.concatMap (validateField trimmedForm) [ Username, Password, Email ] of
        [] ->
            Ok form

        err ->
            Err err


validateField : Form -> Field -> List Problem
validateField form field =
    List.map (InvalidField field) <|
        case field of
            Username ->
                if String.length form.username < 2 then
                    [ "Username too short" ]

                else if String.length form.username > 30 then
                    [ "Username too long" ]

                else
                    []

            Password ->
                if String.length form.password < 2 then
                    [ "Password too short" ]

                else
                    []

            Email ->
                if String.length form.email < 0 then
                    [ "Email too short" ]

                else
                    []



---- VIEW ----


view : Model -> { title : String, content : List (Element Msg) }
view model =
    { title = "Register"
    , content =
        [ column [] (List.map viewProblem model.problems)
        , viewForm model.form
        ]
    }


viewProblem : Problem -> Element Msg
viewProblem problem =
    case problem of
        InvalidField _ txt ->
            text txt

        ServerError error ->
            text error


viewForm : Form -> Element Msg
viewForm form =
    column
        Template.form
        [ Template.username ChangedUsername form.username Nothing
        , Template.newPassword ChangedPassword form.password Nothing

        --, Template.formText ChangedEmail form.email Nothing "Email"
        , Template.formSubmit
            { onPress = Just Submitted
            , label = text "Submit"
            }
        ]
