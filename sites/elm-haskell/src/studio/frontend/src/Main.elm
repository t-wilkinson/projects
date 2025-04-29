module Main exposing (..)

import Browser exposing (..)
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (text)
import Html.Attributes exposing (href)
import Json.Decode exposing (Value)
import Page
import Page.About as About
import Page.Home as Home
import Page.Login as Login
import Page.Studio as Studio
import Route exposing (Route(..))
import Session exposing (Session, User(..))
import Url



--- Model ---

type alias Mod a =
    { model : { a | model : String }
    , cmd : { a | cmd : String }
    , view : a
    }

type Model
    = Home Home.Model
    -- | Login Login.Model
    -- | Studio Studio.Model
    -- | About About.Model
    | NotFound { session : Session }

type Msg
    = NoOp

init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init value url key =
    ( NotFound { session = Session key Guest }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    ( model, Cmd.none )

view : Model -> Document Msg
view model =
        { title = "NotFound", body = [ text "NotFound" ] }

main : Program Value Model Msg
main =
    application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }

-- init : Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
-- init _ url key =
--     fromRoute (Session key Session.Guest) (Route.fromUrl url)



-- --- Update ---


-- type Msg
--     = OnUrlRequest UrlRequest
--     | OnUrlChange Url.Url
--     | HomeMsg Home.Msg
--     -- | LoginMsg Login.Msg
--     -- | StudioMsg Studio.Msg
--     -- | AboutMsg About.Msg


-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case ( msg, model ) of
--         ( OnUrlRequest urlRequest, _ ) ->
--             case urlRequest of
--                 Internal url ->
--                     ( model, Nav.pushUrl (.navKey (toSession model)) (Url.toString url) )

--                 External url ->
--                     ( model, Nav.load url )

--         ( OnUrlChange url, _ ) ->
--             fromRoute (toSession model) (Route.fromUrl url)

--         ( HomeMsg subMsg, Home subModel ) ->
--             toUpdate (Home.update subMsg subModel) Home HomeMsg

-- --         ( LoginMsg subMsg, Login subModel ) ->
-- --             toUpdate (Login.update subMsg subModel) Login LoginMsg

-- --         ( StudioMsg subMsg, Studio subModel ) ->
-- --             toUpdate (Studio.update subMsg subModel) Studio StudioMsg

-- --         ( AboutMsg subMsg, About subModel ) ->
-- --             toUpdate (About.update subMsg subModel) About AboutMsg

--         _ ->
--             ( model, Cmd.none )


-- toSession : Model -> Session
-- toSession model =
--     case model of
--         Home subModel ->
--             subModel.session

-- --         Login subModel ->
-- --             subModel.session

-- --         Studio subModel ->
-- --             subModel.session

-- --         About subModel ->
-- --             subModel.session

--         NotFound subModel ->
--             subModel.session


-- fromRoute : Session -> Route -> ( Model, Cmd Msg )
-- fromRoute session route =
--     case route of
--         Route.Home ->
--             toUpdate (Home.init session) Home HomeMsg

-- --         Route.Login ->
-- --             toUpdate (Login.init session) Login LoginMsg

-- --         Route.Logout ->
-- --             case toUpdate (Home.init (Session.logout session)) Home HomeMsg of
-- --                 ( model, msg ) ->
-- --                     ( model
-- --                     , Cmd.batch
-- --                         [ Nav.pushUrl session.navKey (Route.toString Route.Home)
-- --                         , logout ()
-- --                         ]
-- --                     )

-- --         Route.Studio ->
-- --             toUpdate (Studio.init session) Studio StudioMsg

-- --         Route.About ->
-- --             toUpdate (About.init session) About AboutMsg

--         Route.NotFound ->
--             ( NotFound { session = session }, Cmd.none )


-- toUpdate : ( model, Cmd msg ) -> (model -> Model) -> (msg -> Msg) -> ( Model, Cmd Msg )
-- toUpdate ( model, msg ) toModel toMsg =
--     ( toModel model, Cmd.batch [ Cmd.map toMsg msg, ready () ] )



-- --- View ---


-- view : Model -> Document Msg
-- view model =
--     let
--         view_ =
--             Page.view (toSession model)
--     in
--     case model of
--         Home sub ->
--             view_ HomeMsg <| Home.view sub

-- --         Login sub ->
-- --             view_ LoginMsg <| Login.view sub

-- --         Studio sub ->
-- --             view_ StudioMsg <| Studio.view sub

-- --         About sub ->
-- --             view_ AboutMsg <| About.view sub

--         NotFound _ ->
--             { title = "NotFound", body = [ text "NotFound" ] }



-- --- Subscriptions ---


-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--     case model of
--         _ ->
--             Sub.none



-- --- Ports ---


-- port logout : () -> Cmd msg


-- port ready : () -> Cmd msg



-- --- Program ---


-- main : Program Value Model Msg
-- main =
--     application
--         { view = view
--         , init = init
--         , update = update
--         , subscriptions = subscriptions
--         , onUrlChange = OnUrlChange
--         , onUrlRequest = OnUrlRequest
--         }
