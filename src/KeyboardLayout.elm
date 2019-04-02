module KeyboardLayout exposing (Key, Keyboard, KeyboardModel, keyboardFromModel)

import Dict exposing (Dict)
import HSLuv exposing (HSLuv)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)
import Maybe exposing (andThen)

type alias Key =
    { char : String
    , note : String
    , row : Int
    , colStart : Int
    , colEnd : Int
    , color : (Float, Float, Float)
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
getOrBlank mapping char =
    case Dict.get char mapping of
        Just note ->
            note

        Nothing ->
            ""


prependToRow : KeyboardModel -> Int -> String -> Keyboard -> Keyboard
prependToRow km rowNum nextChar current =
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
    let
        note =
            Dict.get nextChar km.mapping
            |> andThen (\name -> Dict.get name km.noteConfig)
            
    in
    let
        color = if Set.member nextChar km.keysPressed then (0.0, 0.0, 100.0) else 
            case note of
                Just n ->
                    n.color

                Nothing ->
                    (0.0, 0.0, 0.0)
    in
    { char = nextChar, note = getOrBlank km.mapping nextChar, row = rowNum + 1, colStart = start, colEnd = start + width, color = color } :: current


stringToRow : KeyboardModel -> Int -> String -> Keyboard
stringToRow km rowNum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.foldl (prependToRow km rowNum) []


keyboardFromModel : KeyboardModel -> Keyboard
keyboardFromModel km =
    let
        lines =
            String.lines km.layout
    in
    List.indexedMap (stringToRow km) lines
        |> List.concat
