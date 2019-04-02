module Main exposing (Model, init, main, update, view)

import Browser
import Configs exposing (myKeyboard)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (class, id, style, tabindex, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import KeyHtml exposing (Msg(..), UIMode(..), renderKeyboard)
import KeyboardLayout exposing (Keyboard, KeyboardModel, keyboardFromModel)
import KeyboardState exposing (sendActiveNotes, sendNoteConfig)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { keyboard : KeyboardModel
    , uiMode : UIMode
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { keyboard = myKeyboard
      , uiMode = Playing
      }
    , sendNoteConfig myKeyboard.noteConfig
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg oldModel =
    let oldKeyboard = oldModel.keyboard in
    case msg of
        NewLayout s ->
            ( { oldModel | keyboard = { oldKeyboard | layout = s }}, Cmd.none )

        NewNote char s ->
            ( { oldModel | keyboard = { oldKeyboard | mapping = Dict.insert char s oldKeyboard.mapping } }, Cmd.none )

        KeyActive active char ->
            if Set.member char oldKeyboard.keysPressed /= active then
                let
                    newKeysPressed =
                        if active then
                            Set.insert char oldKeyboard.keysPressed

                        else
                            Set.remove char oldKeyboard.keysPressed
                in
                ( { oldModel | keyboard = { oldKeyboard | keysPressed = newKeysPressed } }
                , translateKeyPresses oldKeyboard.mapping oldKeyboard.layout newKeysPressed
                    |> sendActiveNotes
                )

            else
                ( oldModel, Cmd.none )

        StartPlaying ->
            ( { oldModel | uiMode = Playing }, Cmd.none )

        EditLayout ->
            ( { oldModel | uiMode = EditingLayout }, Cmd.none )


translateKeyPresses : Dict String String -> String -> Set String -> List String
translateKeyPresses keyboardMapping layout keys =
    Set.toList keys
        |> List.filter (\k -> String.contains k layout)
        |> List.filterMap (\k -> Dict.get k keyboardMapping)


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder msg =
    field "key" (map msg string)


center : List (Html msg) -> Html msg
center elems =
    elems
        |> List.map List.singleton
        |> List.map (div [ style "text-align" "center", style "display" "block" ])
        |> div []


view : Model -> Html Msg
view oldModel =
    let
        editArea =
            if oldModel.uiMode == Playing then
                center
                    [ button
                        [ onClick EditLayout
                        , style "margin-top" "2px"
                        ]
                        [ text "Edit layout" ]
                    ]

            else
                center
                    [ textarea
                        [ value oldModel.keyboard.layout
                        , onInput NewLayout
                        , style "margin-top" "2px"
                        , style "height" ((String.lines oldModel.keyboard.layout |> List.length  |> toFloat |> (*) 1.2 |> String.fromFloat) ++ "em")
                        ]
                        []
                    , button [ onClick StartPlaying ] [ text "Resume playing" ]
                    ]
    in
    let
        topLevelCallbacks =
            if oldModel.uiMode == Playing then
                [ on "keydown" (keyDecoder <| KeyActive True)
                , on "keyup" (keyDecoder <| KeyActive False)
                , onBlur EditLayout
                , style "background-color" "burlywood"
                ]

            else
                [ onFocus StartPlaying
                , style "background-color" "#ddd"
                ]
    in
    div
        (topLevelCallbacks
            ++ [ tabindex 0
               , id "main"
               , style "height" "100%"
               ]
        )
        [ renderKeyboard (keyboardFromModel oldModel.keyboard)
        , editArea
        ]
