module Template exposing (..)

import Animation exposing (Property, State, Step, left, opacity, style, to)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Route exposing (Route)



---- PALETTE ----


palette =
    { dark = rgb255 40 42 54
    , dark2 = rgb255 60 69 86
    , dark3 = rgb255 42 44 55
    , dark4 = rgb255 29 30 38
    , gray = rgb255 68 71 90
    , gray2 = rgb255 98 114 164
    , light = rgb255 248 248 242
    , cyan = rgb255 139 233 253
    , purple = rgb255 189 147 249
    , warning = rgb255 255 85 85
    , green = rgb255 80 250 123
    }



---- STYLES ----


border : Attribute msg
border =
    Border.roundEach
        { topLeft = 3
        , topRight = 10
        , bottomLeft = 10
        , bottomRight = 3
        }


link : List (Attribute msg)
link =
    [ alignRight
    , Background.color palette.green
    , padding 10
    , border
    ]


gradient : Attribute msg
gradient =
    Background.gradient
        { angle = 0.5
        , steps =
            [ palette.cyan
            , palette.purple
            ]
        }



---- TEMPLATE ----
-- general --


title : String -> Element msg
title txt =
    el
        [ centerX
        , padding 20
        , Font.size 100
        , Font.color palette.cyan
        , Font.family [ Font.typeface "Baloo" ]

        --, Font.shadow
        --    { offset = ( 5, 5 )
        --    , blur = 10.1
        --    , color = palette.purple
        --    }
        ]
        (text txt)


deleteButton : Maybe msg -> Element msg -> Element msg
deleteButton maybeMsg element =
    Input.button
        [ border
        , padding 6
        , Background.color palette.warning
        , Font.size 20
        , Font.color palette.dark4
        , alignRight
        ]
        { onPress = maybeMsg
        , label = element
        }


button : Maybe msg -> String -> Element msg
button maybeMsg label =
    Input.button
        [ border ]
        { onPress = maybeMsg
        , label = text label
        }



-- login --


username : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> Element msg
username toMsg txt maybePlaceholder =
    Input.username
        formInput
        { onChange = toMsg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text "Username")
        }


newPassword : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> Element msg
newPassword toMsg txt maybePlaceholder =
    Input.newPassword
        formInput
        { onChange = toMsg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text "Password")
        , show = False
        }


currentPassword : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> Element msg
currentPassword toMsg txt maybePlaceholder =
    Input.currentPassword
        formInput
        { onChange = toMsg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text "Password")
        , show = False
        }



-- form --


form : List (Attribute msg)
form =
    [ Font.color palette.dark
    , spacing 40
    , width fill
    ]


formInput : List (Attribute msg)
formInput =
    [ Background.color palette.dark
    , Border.width 0
    , Font.color palette.light
    , padding 16
    , Border.roundEach
        { topLeft = 0
        , topRight = 0
        , bottomLeft = 10
        , bottomRight = 3
        }
    ]


formSubmitOnEnter :
    msg
    -> (String -> msg)
    -> String
    -> Maybe (Input.Placeholder msg)
    -> String
    -> Element msg
formSubmitOnEnter subMsg msg txt maybePlaceholder label =
    Input.text
        (formInput ++ [ onEnter subMsg ])
        { onChange = msg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text label)
        }


formText : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> String -> Element msg
formText msg txt maybePlaceholder label =
    Input.text
        formInput
        { onChange = msg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text label)
        }


formTitle : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> String -> Element msg
formTitle msg txt maybePlaceholder label =
    Input.text
        formInput
        { onChange = msg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text label)
        }


formMultiline : (String -> msg) -> String -> Maybe (Input.Placeholder msg) -> String -> Bool -> Element msg
formMultiline msg txt maybePlaceholder label spellcheck =
    Input.multiline
        (formInput
            ++ [ height (px 500)
               ]
        )
        { onChange = msg
        , text = txt
        , placeholder = maybePlaceholder
        , label = formLabel (text label)
        , spellcheck = spellcheck
        }


formLabel : Element msg -> Input.Label msg
formLabel =
    Input.labelAbove
        [ Font.color palette.cyan
        , alignLeft
        , width fill
        , Background.color palette.dark4
        , padding 15
        , moveDown 5
        , Border.roundEach
            { topLeft = 3
            , topRight = 10
            , bottomLeft = 0
            , bottomRight = 0
            }
        ]


labels : List (Attribute msg)
labels =
    [ Font.color palette.cyan
    , alignLeft
    , width fill
    , Background.color palette.dark4
    , padding 15
    , Border.roundEach
        { topLeft = 3
        , topRight = 10
        , bottomLeft = 0
        , bottomRight = 0
        }
    ]


formSubmit : { onPress : Maybe msg, label : Element msg } -> Element msg
formSubmit =
    Input.button
        [ Font.color palette.dark
        , Background.color palette.purple

        --, Background.gradient { angle = 0.5, steps = [ palette.cyan, palette.purple ] }
        , Border.rounded 10
        , spacing 20
        , padding 10
        , border
        ]



---- Blog Posts ----


contentWidth : Attribute msg
contentWidth =
    width
        (fill
            |> maximum 1000
            |> minimum 300
        )


blogPost : List (Element msg) -> Element msg
blogPost =
    column
        [ Background.color palette.dark
        , padding 40
        ]



---- ANIMATION ----


render : State -> List (Attribute msg)
render property =
    List.map htmlAttribute <| Animation.render property


interrupt : List Step -> State -> State
interrupt =
    Animation.interrupt



---- OTHER ----
-- bar --


bar : Element msg
bar =
    el
        [ height (px 3)
        , width fill
        , Background.color palette.gray
        ]
        none


options : { options : List Option }
options =
    { options =
        [ focusStyle
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing
            }
        ]
    }


{-| -}
onEnter : msg -> Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
