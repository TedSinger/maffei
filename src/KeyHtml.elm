module KeyHtml exposing (renderKeyboard)

import Color exposing (toCssString)
import HSLuv exposing (HSLuv)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (attribute, class, id, style, type_, value)
import Html.Events exposing (keyCode, on, onFocus, onInput, targetValue)
import Key exposing (Key, Keyboard, toKeyboard)
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
    ]


spaceStyle : Key -> List (Attribute msg)
spaceStyle key =
    [ class "space"
    , style "grid-row" (String.fromInt key.row)
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


rowWidth row =
    List.map .width row |> List.sum


gridTemplateColumns : Keyboard -> String
gridTemplateColumns keyboard =
    let
        maxCol =
            List.maximum <| List.map rowWidth keyboard
    in
    case maxCol of
        Just n ->
            String.concat [ "repeat(", String.fromInt (n - 1), ", 1fr)" ]

        Nothing ->
            ""

-- WARNING: If you don't order the html of the keys in the 
-- intended display order, placing them appropriately in the 
-- grid becomes super finicky.
renderKeyboard : Keyboard -> Html Msg
renderKeyboard keyboard =
    List.concat keyboard
        |> List.map renderKey
        |> div
            [ id "keyboard"
            , style "grid-template-columns" (gridTemplateColumns keyboard)
            ]
