module Page.Home exposing (Model, Msg, init, update, view)

import Api exposing (Content)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Session exposing (..)
import Templates



--- Model ---


type alias Model =
    { session : Session
    , contents : Status (List Api.Content)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , contents = Loading
      }
    , Cmd.none
    )



--- Update ---


type Msg
    = GotContent (Result Http.Error (List Content))
    | GotProgress Http.Progress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotContent (Ok contents) ->
            ( { model | contents = Recieved contents }, Cmd.none )

        GotContent (Err error) ->
            ( { model | contents = Failed error }, Cmd.none )

        GotProgress progress ->
            ( model, Cmd.none )



--- View ---


type alias Section =
    Model -> Html Msg


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        main_
            [ id "home" ]
            [ viewIntro model
            , viewExplore model
            , viewVideos model
            , viewProjects model
            , viewTestimonials model
            ]
    }



-- Intro


viewIntro : Section
viewIntro model =
    h1 [ id "intro" ] [ text "Book Studio Time" ]



-- Explore


viewExplore : Section
viewExplore model =
    section
        [ id "explore" ]
        [ p [] [ text "see more" ]
        -- , Route.link Route.Studio []
        ]



-- Videos


viewVideos : Section
viewVideos model =
    section
        [ id "videos" ]
        [ h2 [] [ text "Obi Wan Kenobi" ]
        , div [ class "video" ]
            [ iframe [ src "https://www.youtube.com/embed/0RHAV51R1Kc" ] []
            ]
        ]



-- Project


viewProjects : Section
viewProjects model =
    section
        [ id "projects" ]
        [ h2 [] [ text "Projects" ]
        , ul [ class "projects" ]
            [ viewFigure
                "https://i.pinimg.com/originals/a9/c5/72/a9c5729ce60c92a89df50c91a5c52a4f.jpg"
                "This is a description"
                "This is a description"
            , viewFigure
                "https://i2.wp.com/t2.genius.com/unsafe/544x0/https%3A%2F%2Fimages.genius.com%2F70a7ef69242915a9cd1c3f68031e4c0a.1000x1000x1.jpg?w=640&ssl=1"
                "This is a description"
                "another one"
            , viewFigure
                "https://thelodascreative.com/wp-content/uploads/2018/01/Issa_21Savage-e1515808321271.png"
                "This is a description"
                "This is a description"
            , viewFigure
                "https://i.pinimg.com/originals/80/9d/a5/809da545a9e9d24668512c29c72c0817.jpg"
                "This is a description"
                "Died so his ghost writes now"
            , viewFigure
                "https://d279m997dpfwgl.cloudfront.net/wp/2018/12/JColeKOD.jpg"
                "This is a description"
                "I love this one"
            , viewFigure
                "https://pm1.narvii.com/6874/14c4c75bac68ebb1f53006934902b19c42798c1fr1-591-595v2_uhq.jpg"
                "This is a description"
                "Bobby"
            , viewFigure "https://images-cdn.9gag.com/photo/aN1Ajy4_460s.jpg"
                "This is a description"
                "Rest In Jail"
            , viewFigure "https://images.contentstack.io/v3/assets/blt1b60905dd65bfb9b/blt34bda9ac79d55d02/5b06688aaaa7eb966fe370e3/tyler-the-creator-flower-boy-album-cover.jpg"
                "This is a description"
                "He's a flower boy"
            ]
        ]


viewFigure : String -> String -> String -> Html Msg
viewFigure source alternate caption =
    figure [ class "js-tilt", attribute "data-tilt" "" ]
        [ img [ alt alternate, src source ] []
        , figcaption [] [ h3 [] [ text caption ] ]
        ]



-- Testimonials


viewTestimonials : Section
viewTestimonials model =
    section
        [ id "testimonials" ]
        [ h2 [] [ text "Testimonials" ]
        , div [ class "testimonials" ]
            [ viewTestimonial "It was good yo" "the platapus band"
            , viewTestimonial "It was awesome" "fairy sniffers"
            , viewTestimonial "10/10 would recommend" "bob dylan"
            , viewTestimonial "it was quite a cool place" "will"
            , viewTestimonial "I would definitely go there again" "robert"
            , viewTestimonial "yeh bro" "bro"
            ]
        ]


viewTestimonial : String -> String -> Html Msg
viewTestimonial quote band =
    blockquote []
        [ text quote
        , footer []
            [ Html.cite [] [ a [] [ text band ] ]
            ]
        ]



-- Content


viewContent : Section
viewContent model =
    section
        [ id "content" ]
        [ case model.contents of
            Failed error ->
                text <| Templates.viewError error

            Recieved contents ->
                ul [] <| (List.map viewContent_ << List.reverse) <| contents

            Loading ->
                text "loading ..."
        ]


viewContent_ : Content -> Html Msg
viewContent_ content =
    li
        []
        [ span [] [ text content.contentHeading ]
        , text " : "
        , text content.contentBody
        ]
