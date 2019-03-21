module KeyHtml exposing (Msg(..), renderKeyboard)

import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, id, style, type_, value)
import Html.Events exposing (keyCode, on, onInput, targetValue)
import KeyboardLayout exposing (Key, Keyboard)
import Set exposing (Set)


type Msg
    = NewLayout String
    | NewNote String String
    | KeyActive Bool String


noteInputStyle key =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-color" "black"
    , style "border-radius" "3px"
    , style "height" "15px"
    , style "text-align" "center"
    , style "width" "100%"
    , style "-webkit-box-sizing" "border-box"
    , style "-moz-box-sizing" "border-box"
    , style "box-sizing" "border-box"
    , type_ "text"
    , value key.note
    , onInput (NewNote key.char)
    ]


charStyle =
    [ style "align" "left"
    ]


keyStyle : Key -> Bool -> List (Attribute msg)
keyStyle key isActive =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-radius" "5px"
    , style "margin-left" "8%"
    , style "margin-right" "8%"
    , style "height" "40px"
    , style "font-family" "monospace"
    , style "background-color"
        (if isActive then
            "cyan"

         else
            "peachpuff"
        )
    , style "grid-row" (String.fromInt key.row)
    , style "grid-column-start" (String.fromInt key.colStart)
    , style "grid-column-end" (String.fromInt key.colEnd)
    ]


spaceStyle : Key -> List (Attribute msg)
spaceStyle key =
    [ style "border-color" "white"
    , style "color" "white"
    , style "height" "40px"
    , style "grid-row" (String.fromInt key.row)
    , style "grid-column-start" (String.fromInt key.colStart)
    , style "grid-column-end" (String.fromInt key.colEnd)
    ]


renderKey : Set String -> Key -> Html Msg
renderKey keysPressed key =
    if key.char == " " then
        div (spaceStyle key) []

    else
        div (keyStyle key <| Set.member key.char keysPressed)
            [ input (noteInputStyle key) []
            , div charStyle [ text key.char ]
            ]


gridTemplateColumns : Keyboard -> String
gridTemplateColumns keyboard =
    let
        maxCol =
            List.maximum <| List.map .colEnd keyboard
    in
    case maxCol of
        Just n ->
            String.concat [ "repeat(", String.fromInt (n - 1), ", 1fr)" ]

        Nothing ->
            ""



-- CSS grid placement assumes that item order is relevant to placement


compareKeys : Key -> Key -> Order
compareKeys left right =
    if left.row > right.row then
        GT

    else if left.row < right.row then
        LT

    else if left.colEnd > right.colEnd then
        GT

    else if left.colEnd < right.colEnd then
        LT

    else
        EQ


renderKeyboard : Keyboard -> Set String -> Html Msg
renderKeyboard keyboard keysPressed =
    List.sortWith compareKeys keyboard
        |> List.map (renderKey keysPressed)
        |> div
            [ style "display" "grid"
            , style "justify-content" "start"
            , style "grid-template-columns" (gridTemplateColumns keyboard)
            ]
