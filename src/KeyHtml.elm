module KeyHtml exposing (Msg(..), renderKeyboard)

import Html exposing (Attribute, Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (attribute, class, id, style, tabindex, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, targetValue)
import KeyboardLayout exposing (Key, KeyPlacement, Keyboard)
import Set exposing (Set)


type Msg
    = NewLayout String
    | NewNote String String
    | KeyDown String
    | KeyUp String


noteInputStyle =
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
    ]


charStyle =
    [ style "align" "left"
    ]


keyStyle : KeyPlacement -> Bool -> List (Attribute msg)
keyStyle keyPlace isActive =
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
    , style "grid-row" (String.fromInt <| keyPlace.row + 1)
    , style "grid-column-start" (String.fromInt keyPlace.colStart)
    , style "grid-column-end" (String.fromInt keyPlace.colEnd)
    ]


spaceStyle : KeyPlacement -> List (Attribute msg)
spaceStyle keyPlace =
    [ style "border-color" "white"
    , style "color" "white"
    , style "height" "40px"
    , style "grid-row" (String.fromInt <| keyPlace.row + 1)
    , style "grid-column-start" (String.fromInt keyPlace.colStart)
    , style "grid-column-end" (String.fromInt keyPlace.colEnd)
    ]


renderKey : Set String -> ( Key, KeyPlacement ) -> Html Msg
renderKey keysPressed ( key, keyPlace ) =
    if key.char == " " then
        div (spaceStyle keyPlace) []

    else
        div (keyStyle keyPlace <| Set.member key.char keysPressed)
            [ input
                (noteInputStyle
                    ++ [ type_ "text"
                       , value key.note
                       , onInput (NewNote key.char)
                       ]
                )
                []
            , div charStyle [ text key.char ]
            ]


getMaxColumn : Keyboard -> String
getMaxColumn keyboard =
    let
        maxCol =
            List.maximum <| List.map (\( key, kp ) -> kp.colEnd) keyboard
    in
    case maxCol of
        Just n ->
            String.concat [ "repeat(", String.fromInt (n - 1), ", 1fr)" ]

        Nothing ->
            ""


renderKeyboard : Keyboard -> Set String -> Html Msg
renderKeyboard keyboard keysPressed =
    List.map (renderKey keysPressed) keyboard
        |> div
            [ style "display" "grid"
            , style "justify-content" "start"
            , style "grid-template-columns" (getMaxColumn keyboard)
            ]
