module Main exposing (Model, init, main, update, view)

import Browser
import Configs exposing (myLayout, myNoteCfg, myNotesDict)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, button, div, input, text, textarea)
import Html.Attributes exposing (class, id, style, tabindex, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput)
import Json.Decode exposing (Decoder, field, map, string)
import Json.Encode as E
import KeyHtml exposing (Msg(..), UIMode(..), renderKeyboard)
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
    { layout : String
    , notesDict : Dict String String
    , keysPressed : Set String
    , uiMode : UIMode
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { layout = myLayout
      , notesDict = myNotesDict
      , keysPressed = Set.empty
      , uiMode = Playing
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
            if Set.member char currentModel.keysPressed /= active then
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

            else
                ( currentModel, Cmd.none )

        StartPlaying ->
            ( { currentModel | uiMode = Playing }, Cmd.none )

        EditLayout ->
            ( { currentModel | uiMode = EditingLayout }, Cmd.none )


translateKeyPresses : Dict String String -> Set String -> List String
translateKeyPresses notes keys =
    Set.toList keys
        |> List.filterMap (\k -> Dict.get k notes)


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
view currentModel =
    let
        editArea =
            if currentModel.uiMode == Playing then
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
                        [ value currentModel.layout
                        , onInput NewLayout
                        , style "margin-top" "2px"
                        ]
                        []
                    , button [ onClick StartPlaying ] [ text "Resume playing" ]
                    ]
    in
    let
        topLevelCallbacks =
            if currentModel.uiMode == Playing then
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
        [ renderKeyboard (keyboardFromModel currentModel.notesDict currentModel.layout) currentModel.keysPressed
        , editArea
        ]
