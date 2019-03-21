module Main exposing (Model, init, main, update, view)

import Browser
import Configs exposing (myLayout, myNoteCfg, myNotesDict)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, input, text, textarea)
import Html.Attributes exposing (class, id, style, tabindex, value)
import Html.Events exposing (on, onInput)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import KeyHtml exposing (Msg(..), renderKeyboard)
import KeyboardLayout exposing (Keyboard, keyboardFromModel)
import KeyboardState exposing (sendActiveNotes, sendNoteCfg)
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
    { layout : String, notesDict : Dict String String, keysPressed : Set String }


init : () -> ( Model, Cmd msg )
init _ =
    ( { layout = myLayout
      , notesDict = myNotesDict
      , keysPressed = Set.empty
      }
    , sendNoteCfg myNoteCfg
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg currentModel =
    case msg of
        NewLayout s ->
            ( { currentModel | layout = s }, Cmd.none )

        NewNote char s ->
            ( { currentModel | notesDict = Dict.insert char s currentModel.notesDict }, Cmd.none )

        KeyActive active char ->
            let
                newKeysPressed =
                    if active then
                        Set.insert char currentModel.keysPressed

                    else
                        Set.remove char currentModel.keysPressed
            in
            ( { currentModel | keysPressed = newKeysPressed }
            , translateKeyPresses currentModel.notesDict newKeysPressed
                |> sendActiveNotes
            )


translateKeyPresses : Dict String String -> Set String -> List String
translateKeyPresses notes keys =
    Set.toList keys
        |> List.filterMap (\k -> Dict.get k notes)


keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder msg =
    field "key" (map msg string)


view : Model -> Html Msg
view currentModel =
    div
        [ on "keydown" (keyDecoder <| KeyActive True)
        , on "keyup" (keyDecoder <| KeyActive False)
        , tabindex 0
        , id "main"
        ]
        [ textarea [ value currentModel.layout, onInput NewLayout ] []
        , renderKeyboard (keyboardFromModel currentModel.notesDict currentModel.layout) currentModel.keysPressed
        ]
