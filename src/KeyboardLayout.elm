module KeyboardLayout exposing (Key, Keyboard, KeyboardModel, keyboardFromModel)

import Dict exposing (Dict)
import HSLuv exposing (HSLuv)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)

type alias Key =
    { char : String
    , note : String
    , row : Int
    , colStart : Int
    , colEnd : Int
    , color : Int
    }


type alias Keyboard =
    List Key

type alias KeyboardModel =     
    { layout : String
    , mapping : Dict String String
    , keysPressed : Set String
    , noteConfig : NoteConfig
    }


getOrBlank : Dict String String -> String -> String
getOrBlank keyboardMapping char =
    case Dict.get char keyboardMapping of
        Just note ->
            note

        Nothing ->
            ""


prependToRow : Dict String String -> Int -> String -> Keyboard -> Keyboard
prependToRow keyboardMapping rowNum nextChar current =
    let
        key =
            {}
    in
    let
        width =
            if nextChar == " " then
                1

            else
                2
    in
    let
        start =
            case current of
                headKey :: tail ->
                    headKey.colEnd

                [] ->
                    1
    in
    { char = nextChar, note = getOrBlank keyboardMapping nextChar, row = rowNum + 1, colStart = start, colEnd = start + width, color = 0 } :: current


stringToRow : Dict String String -> Int -> String -> Keyboard
stringToRow keyboardMapping rowNum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.foldl (prependToRow keyboardMapping rowNum) []


keyboardFromModel : KeyboardModel -> Keyboard
keyboardFromModel km =
    let
        lines =
            String.lines km.layout
    in
    List.indexedMap (stringToRow km.mapping) lines
        |> List.concat
