module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (id, style, tabindex, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, targetValue)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
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
    "azsxdcfvgb1q2w3e4r5t6hnjmk,l.;y7u8i9o0p-[=]"


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


type Msg
    = NewLayout String
    | NewNote String String
    | KeyDown String
    | KeyUp String


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
        , toKeyboard currentModel.layout currentModel.notesDict currentModel.keysPressed
        ]


keyStyle : Bool -> List (Attribute msg)
keyStyle active =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-radius" "5px"
    , style "float" "left"
    , style "margin" "0.4%"
    , style "width" "6%"
    , style "height" "40px"
    , style "position" "relative"
    , style "font-family" "monospace"
    , style "background-color"
        (if active then
            "cyan"

         else
            "peachpuff"
        )
    ]


spaceStyle =
    [ style "width" "21px"
    , style "float" "left"
    , style "border-color" "white"
    , style "color" "white"
    , style "height" "40px"
    , style "position" "relative"
    ]


noteToText : Maybe String -> String
noteToText mnote =
    case mnote of
        Just note ->
            note

        Nothing ->
            ""


noteInputStyle =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-color" "black"
    , style "border-radius" "3px"
    , style "max-width" "80%"
    , style "height" "15px"
    , style "text-align" "center"
    , style "margin-left" "10%"
    , style "margin-right" "10%"
    , style "-webkit-box-sizing" "border-box"
    , style "-moz-box-sizing" "border-box"
    , style "box-sizing" "border-box"
    ]


symbolStyle =
    [ style "align" "left", style "position" "absolute", style "bottom" "0" ]


toKey : String -> Bool -> Maybe String -> Html Msg
toKey char active mnote =
    if char == " " then
        div spaceStyle []

    else
        div (keyStyle active)
            [ input (noteInputStyle ++ [ type_ "text", value <| noteToText mnote, onInput (NewNote char) ]) []
            , div symbolStyle [ text char ]
            ]


toRow : String -> Dict String String -> Set String -> Html Msg
toRow s nd kp =
    String.toList s
        |> List.map String.fromChar
        |> List.map (\key -> toKey key (Set.member key kp) <| Dict.get key nd)
        |> div [ style "clear" "left" ]


toKeyboard : String -> Dict String String -> Set String -> Html Msg
toKeyboard s nd kp =
    String.lines s
        |> List.map (\line -> toRow line nd kp)
        |> div []
