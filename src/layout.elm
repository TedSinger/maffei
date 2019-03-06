module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, label, span, text, textarea)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)


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
    { layout : String, notesDict : Dict Char String }


init : () -> ( Model, Cmd msg )
init _ =
    ( { layout = "1234567890-=\n qwertyuiop[]\n  asdfghjkl;'\n   zxcvbnm,./"
      , notesDict = Dict.fromList [ ( 'q', "A#" ) ]
      }
    , Cmd.none
    )


type Msg
    = NewLayout String
    | NewNote Char String


update : Msg -> Model -> ( Model, Cmd msg )
update msg foo =
    case msg of
        NewLayout s ->
            ( { foo | layout = s }, Cmd.none )

        NewNote c s ->
            ( { foo | notesDict = Dict.insert c s foo.notesDict }, Cmd.none )


view : Model -> Html Msg
view foo =
    div []
        [ textarea [ value foo.layout, onInput NewLayout ] []
        , toKeyboard foo.layout foo.notesDict
        ]


keyStyle =
    [ style "border-style" "solid"
    , style "border-width" "1px"
    , style "border-radius" "5px"
    , style "float" "left"
    , style "margin" "0.4%"
    , style "width" "7%"
    , style "height" "40px"
    , style "position" "relative"
    , style "font-family" "monospace"
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


toKey : Char -> Maybe String -> Html Msg
toKey c mnote =
    if c == ' ' then
        div spaceStyle []

    else
        div keyStyle
            [ input (noteInputStyle ++ [ type_ "text", value <| noteToText mnote, onInput (NewNote c) ]) []
            , div symbolStyle [ text (String.fromChar c) ]
            ]


toRow : String -> Dict Char String -> Html Msg
toRow s nd =
    s
        |> String.toList
        |> List.map (\key -> toKey key <| Dict.get key nd)
        |> div [ style "clear" "left" ]


toKeyboard : String -> Dict Char String -> Html Msg
toKeyboard s nd =
    s
        |> String.lines
        |> List.map (\line -> toRow line nd)
        |> div []
