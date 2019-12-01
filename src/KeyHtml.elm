module KeyHtml exposing (renderKeyboard)

import Color exposing (toCssString)
import HSLuv exposing (HSLuv)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, class, id, style, type_, value)
import Html.Events exposing (keyCode, on, onFocus, onInput, targetValue)
import KeyboardLayout exposing (Key, Keyboard)
import Msg exposing (Msg(..))
import Set exposing (Set)


noteInputStyle key =
    [ class "note"
    , type_ "text"
    , value key.note
    , onInput (NewNote key.char)
    , onFocus EditLayout
    ]


hsLuvToString : ( Float, Float, Float ) -> String
hsLuvToString color =
    let
        ( red, green, blue ) =
            HSLuv.hpluvToRgb color
    in
    HSLuv.rgba { red = red, green = green, blue = blue, alpha = 1 }
        |> HSLuv.toColor
        |> toCssString


keyStyle : Key -> List (Attribute msg)
keyStyle key =
    [ class "key"
    , style "background-color" (hsLuvToString key.color)
    , style "grid-row" (String.fromInt key.row)
    , style "grid-column-start" (String.fromInt key.colStart)
    , style "grid-column-end" (String.fromInt key.colEnd)
    ]


spaceStyle : Key -> List (Attribute msg)
spaceStyle key =
    [ class "space"
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
            , div [ class "char" ] [ text key.char ]
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
