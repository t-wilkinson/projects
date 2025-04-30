module Page exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Session exposing (Session)
import Templates


view : Session -> (subMsg -> msg) -> { title : String, content : Html subMsg } -> Browser.Document msg
view session toMsg { title, content } =
    { title = "Studio" ++ title
    , body =
        [ viewHeader session
        , Html.map toMsg content
        , viewFooter
        ]
    }


logo : Html msg
logo =
    Route.link Route.Home
        [ img [ src "/images/logo.svg" ] []
        , h1 [] [ text "Klean Studios" ]
        ]



-- Header


viewHeader : Session -> Html msg
viewHeader session =
    let
        link route label =
            li [] [ Route.link route [ text label ] ]

        links =
            [ --link Route.About "About"
            -- , link Route.Studio "Studio"
            -- , if Session.isLoggedIn session then
            --     link Route.Logout "Logout"

            --   else
                -- link Route.Login "Login/Register"
            ]
    in
    header
        []
        [ logo
        , nav [] [ ul [] links ]
        , aside []
            [ input [ type_ "checkbox" ] []
            , div [ class "checkbox" ]
                [ div [ class "hamburger" ] [ span [] [], span [] [], span [] [] ]
                , div [ class "cross" ] [ span [] [], span [] [] ]
                ]
            , nav []
                [ ul [] <|
                    li [] [ Route.link Route.Home [ text "Home" ] ]
                        :: links
                ]
            ]
        ]



-- Footer


viewFooter : Html msg
viewFooter =
    footer [ id "footer" ]
        [ article [ class "social-media" ]
            [ h3 [] [ text "Social Media" ]
            , ul []
                [ viewSocialMedia "Soundcloud"
                    "@micheal_klean"
                    "https://soundcloud.com/michael_klean/tracks"
                , viewSocialMedia "Youtube"
                    "@Max K"
                    "https://www.youtube.com/channel/UCYiQt_7i1a-wz-533kTYNZQ?view_as=subscriber"
                , viewSocialMedia "Twitter"
                    "@mtklim20"
                    "https://twitter.com/mtklim20"
                , viewSocialMedia "Instagram"
                    "@michael_klean"
                    ""
                ]
            ]
        ]


viewSocialMedia : String -> String -> String -> Html msg
viewSocialMedia platform at url =
    li []
        [ a
            [ href url, target "_blank" ]
            [ strong [] [ text platform ]
            , span [] [ text at ]
            ]
        ]
