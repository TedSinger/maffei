module KeyHtml exposing (Msg(..), UIMode(..), renderKeyboard)

import HSLuv exposing (HSLuv)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, id, style, type_, value)
import Html.Events exposing (keyCode, on, onFocus, onInput, targetValue)
import KeyboardLayout exposing (Key, Keyboard)
import Set exposing (Set)
import Color exposing (toCssString)


type Msg
    = NewLayout String
    | NewNote String String
    | KeyActive Bool String
    | StartPlaying
    | EditLayout


type UIMode
    = Playing
    | EditingLayout


noteInputStyle key =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-color" "#aaa"
    , style "border-radius" "3px"
    , style "margin" "5%"
    , style "height" "15px"
    , style "text-align" "center"
    , style "width" "90%"
    , style "-webkit-box-sizing" "border-box"
    , style "-moz-box-sizing" "border-box"
    , style "box-sizing" "border-box"
    , type_ "text"
    , value key.note
    , onInput (NewNote key.char)
    , onFocus EditLayout
    ]


charStyle =
    [ style "position" "relative"
    , style "left" "10%"
    , style "top" "10%"
    ]


hsLuvToString : (Float, Float, Float) -> String
hsLuvToString color =
    
    let (red, green, blue) = HSLuv.hpluvToRgb color in
    HSLuv.rgba {red = red, green = green, blue = blue, alpha = 1}
    |> HSLuv.toColor
    |> toCssString


keyStyle : Key -> List (Attribute msg)
keyStyle key =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-radius" "5px"
    , style "margin-left" "8%"
    , style "margin-right" "8%"
    , style "margin-bottom" "1px"
    , style "margin-top" "2px"
    , style "height" "40px"
    , style "font-family" "monospace"
    , style "box-shadow" "1px 2px #aaa"
    , style "background-color" (hsLuvToString key.color)
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


renderKey : Key -> Html Msg
renderKey key =
    if key.char == " " then
        div (spaceStyle key) []

    else
        div (keyStyle key)
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


renderKeyboard : Keyboard -> Html Msg
renderKeyboard keyboard =
    List.sortWith compareKeys keyboard
        |> List.map renderKey
        |> div
            [ style "display" "grid"
            , style "justify-content" "start"
            , style "grid-template-columns" (gridTemplateColumns keyboard)
            ]
