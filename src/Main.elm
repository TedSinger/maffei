module Main exposing (Model, init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (class, id, style, tabindex, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, targetValue)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import KeyHtml exposing (Msg(..), renderKeyboard)
import KeyboardLayout exposing (Keyboard, keyboardFromModel)
import KeyboardState exposing (activeNotes, noteCfg)
import Notes exposing (genNoteCfg, getName)
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


initNoteCfg : E.Value
initNoteCfg =
    List.range -24 36
        |> List.map genNoteCfg
        |> Dict.fromList
        |> E.dict identity (E.dict identity E.float)


myKeys =
    "zxcvbasdfgqwert123456nm,.hjkl;yuiop[]7890-="


initNotesDict =
    List.range 0 45
        |> List.map (\i -> ( String.slice i (i + 1) myKeys, getName (i - 14) ))
        |> Dict.fromList


init : () -> ( Model, Cmd msg )
init _ =
    ( { layout = "123456 7890-=\n qwert yuiop[]\n  asdfg hjkl;\n   zxcvb nm,."
      , notesDict = initNotesDict
      , keysPressed = Set.empty
      }
    , noteCfg initNoteCfg
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg currentModel =
    case msg of
        NewLayout s ->
            ( { currentModel | layout = s }, Cmd.none )

        NewNote char s ->
            ( { currentModel | notesDict = Dict.insert char s currentModel.notesDict }, Cmd.none )

        KeyDown char ->
            if Set.member char currentModel.keysPressed then
                ( currentModel, Cmd.none )

            else
                applyKeyPress Set.insert char currentModel

        KeyUp char ->
            if Set.member char currentModel.keysPressed then
                applyKeyPress Set.remove char currentModel

            else
                ( currentModel, Cmd.none )


applyKeyPress : (String -> Set String -> Set String) -> String -> Model -> ( Model, Cmd msg )
applyKeyPress op char currentModel =
    let
        newStateOfWorld =
            op char currentModel.keysPressed
    in
    ( { currentModel | keysPressed = newStateOfWorld }
    , newStateOfWorld
        |> translateKeyPresses currentModel.notesDict
        |> E.list E.string
        |> activeNotes
    )


translateKeyPresses : Dict String String -> Set String -> List String
translateKeyPresses notes keys =
    Set.toList keys
        |> List.filterMap (\k -> Dict.get k notes)


keyUpDecoder : Decoder Msg
keyUpDecoder =
    field "key" (map KeyUp string)


keyDownDecoder : Decoder Msg
keyDownDecoder =
    field "key" (map KeyDown string)


view : Model -> Html Msg
view currentModel =
    div [ on "keydown" keyDownDecoder, on "keyup" keyUpDecoder, tabindex 0, id "main" ]
        [ textarea [ value currentModel.layout, onInput NewLayout ] []
        , renderKeyboard (keyboardFromModel currentModel.notesDict currentModel.layout) currentModel.keysPressed
        ]
