module Main exposing (init, main, view)

import Browser
import Configs exposing (myKeyMapping, myLayout, myNoteCfg)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (class, id, style, tabindex, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import KeyHtml exposing (renderKeyboard)
import Key exposing (Keyboard, keyboardFromModel)
import KeyboardState exposing (sendActiveNotes, sendNoteConfig)
import Model exposing (Model, UIMode(..), withKeyChange)
import Msg exposing (Msg(..), update)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , subscriptions = \m -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( { keyLayout = myLayout
      , keyMapping = myKeyMapping
      , keysPressed = Set.empty
      , noteConfig = myNoteCfg
      , uiMode = Playing
      }
    , sendNoteConfig myNoteCfg
    )


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder msg =
    field "key" (map msg string)


center : List (Html msg) -> Html msg
center elems =
    elems
        |> List.map List.singleton
        |> List.map (div [ class "centered" ])
        |> div []


topLevelCallbacks : UIMode -> List (Attribute Msg)
topLevelCallbacks uiMode =
    if uiMode == Playing then
        [ on "keydown" (keyDecoder <| KeyActive True)
        , on "keyup" (keyDecoder <| KeyActive False)
        , onBlur EditLayout
        ]

    else
        [ onFocus StartPlaying ]


getHeight : String -> String
getHeight layout =
    (String.lines layout |> List.length |> toFloat |> (*) 1.2 |> String.fromFloat) ++ "em"


view : Model -> Html Msg
view oldModel =
    let
        editArea =
            if oldModel.uiMode == Playing then
                center
                    [ button
                        [ onClick EditLayout ]
                        [ text "Edit layout" ]
                    ]

            else
                center
                    [ textarea
                        [ value oldModel.keyLayout
                        , onInput NewLayout
                        , style "height" (getHeight oldModel.keyLayout)
                        ]
                        []
                    , button [ onClick StartPlaying ] [ text "Resume playing" ]
                    ]
    in
    div
        (topLevelCallbacks oldModel.uiMode ++ [ tabindex 0, id "main" ])
        [ renderKeyboard (keyboardFromModel oldModel)
        , editArea
        ]
