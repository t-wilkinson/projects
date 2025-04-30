module Page.Login exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Session exposing (..)
import Templates
import Validate exposing (Validator, firstError, fromValid, ifBlank, validate)



--- Model ---


type alias Model =
    { session : Session
    , form : Form
    , error : List (Error FormField)
    , errors : List Http.Error
    }


type alias Form =
    { username : String
    , password : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , form = initForm
      , error = []
      , errors = []
      }
    , Cmd.none
    )


initForm : Form
initForm =
    Form "" ""


type FormField
    = Username
    | Password



--- Update ---


type Msg
    = ChangedForm (Form -> String -> Form) String
    | SubmitLogin
    | SubmittedLogin (Result Http.Error String)
    | SubmitRegister
    | SubmittedRegister (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        login =
            Session.login model.session model.form.username
    in
    case msg of
        ChangedForm func field ->
            ( { model | form = func model.form field }
            , Cmd.none
            )

        SubmitLogin ->
            case validate formValidator model.form of
                Ok validated ->
                    let
                        valid =
                            fromValid validated

                        form =
                            { loginUsername = model.form.username
                            , loginPassword = model.form.password
                            }
                    in
                    ( model
                      -- , Api.postLogin form SubmittedLogin
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = error }, Cmd.none )

        SubmittedLogin (Ok result) ->
            ( { model | session = login }
            , Nav.pushUrl model.session.navKey (Route.toString Route.Home)
            )

        SubmittedLogin (Err error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        SubmitRegister ->
            case validate formValidator model.form of
                Ok validated ->
                    let
                        valid =
                            fromValid validated

                        form =
                            { registerUsername = model.form.username
                            , registerPassword = model.form.password
                            }
                    in
                    ( { model | form = initForm }
                      -- , Api.postRegister form SubmittedRegister
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = error }, Cmd.none )

        SubmittedRegister (Ok result) ->
            ( { model | session = login }
            , Nav.pushUrl model.session.navKey (Route.toString Route.Home)
            )

        SubmittedRegister (Err error) ->
            ( { model | errors = error :: model.errors }, Cmd.none )


formValidator : Validator (Error FormField) Form
formValidator =
    Validate.all
        [ ifBlank .username ( Username, "Please enter a name." )
        , ifBlank .password ( Password, "Please enter a valid email" )
        ]



--- View ---


type alias Section =
    Model -> Html Msg


view : Model -> { title : String, content : Html Msg }
view model =
    { title = ""
    , content =
        main_
            [ id "login"
            , class "register"
            ]
        <|
            [ viewLoginForm model
            , viewRegisterForm model
            ]
                ++ List.map (text << Templates.viewError) model.errors
    }



-- Form


viewLoginForm : Section
viewLoginForm { error, form } =
    let
        viewInput toField =
            Templates.viewInput ChangedForm error (toField form)

        viewFormErrors =
            Templates.viewFormErrors error
    in
    Html.form
        [ id "form-login"
        , onSubmit SubmitLogin
        ]
        [ h1 [] [ text "Register" ]

        -- Switch to login form
        , a [ class "js-toggle-form", href "" ] [ text "(Sign in instead)" ]

        -- Username
        , viewInput .username Username (\f v -> { f | username = v }) "Username" "text"

        -- Password
        , viewInput .password Password (\f v -> { f | password = v }) "Password" "password"

        -- Submit
        , button [] [ text "Submit" ]
        ]


viewRegisterForm : Section
viewRegisterForm { error, form } =
    let
        viewInput toField =
            Templates.viewInput ChangedForm error (toField form)

        viewFormErrors =
            Templates.viewFormErrors error
    in
    Html.form
        [ id "form-register"
        , onSubmit SubmitRegister
        ]
        [ h1 [] [ text "Login" ]

        -- Switch to register form
        , a [ class "js-toggle-form", href "" ] [ text "(Create an account instead)" ]

        -- Username
        , viewInput .username Username (\f v -> { f | username = v }) "Username" "text"

        -- Password
        , viewInput .password Password (\f v -> { f | password = v }) "Password" "password"

        -- Submit
        , button [] [ text "Submit" ]
        ]
