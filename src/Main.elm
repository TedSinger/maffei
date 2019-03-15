module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (style, tabindex, type_, value, id)
import Html.Events exposing (keyCode, on, onClick, onInput, onMouseEnter, targetValue)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import Keystate exposing (activeNotes, noteCfg)
import Set exposing (Set)
import Notes exposing (genNoteCfg, getName)


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

myKeys = "azsxdcfvgb1q2w3e4r5t6hnjmk,l.;y7u8i9o0p-[=]"
initNotesDict = List.range 0 45 
    |> List.map (\i -> (String.slice i (i+1) myKeys, getName (i - 14)))
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
update msg foo =
    case msg of
        NewLayout s ->
            ( { foo | layout = s }, Cmd.none )

        NewNote c s ->
            ( { foo | notesDict = Dict.insert c s foo.notesDict }, Cmd.none )

        KeyDown c ->
            if Set.member c foo.keysPressed then
                ( foo, Cmd.none )

            else
                applyKeyPress Set.insert c foo

        KeyUp c ->
            if Set.member c foo.keysPressed then
                applyKeyPress Set.remove c foo

            else
                ( foo, Cmd.none )


applyKeyPress : (String -> Set String -> Set String) -> String -> Model -> ( Model, Cmd msg )
applyKeyPress op c foo =
    let newStateOfWorld = op c foo.keysPressed in
    ( { foo | keysPressed = newStateOfWorld }
    , newStateOfWorld
        |> translateKeyPresses foo.notesDict
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
view foo =
    div [ on "keydown" keyDownDecoder, on "keyup" keyUpDecoder, tabindex 0, id "main" ]
        [ textarea [ value foo.layout, onInput NewLayout ] []
        , toKeyboard foo.layout foo.notesDict foo.keysPressed
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
    , style "background-color" (if active then "cyan" else "peachpuff")
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
toKey c active mnote =
    if c == " " then
        div spaceStyle []

    else
        div (keyStyle active)
            [ input (noteInputStyle ++ [ type_ "text", value <| noteToText mnote, onInput (NewNote c) ]) []
            , div symbolStyle [ text c ]
            ]


toRow : String -> Dict String String -> Set String -> Html Msg
toRow s nd kp =
    s
        |> String.toList
        |> List.map String.fromChar
        |> List.map (\key -> toKey key (Set.member key kp) <| Dict.get key nd)
        |> div [ style "clear" "left" ]


toKeyboard : String -> Dict String String -> Set String -> Html Msg
toKeyboard s nd kp =
    s
        |> String.lines
        |> List.map (\line -> toRow line nd kp)
        |> div []
