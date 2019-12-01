module KeyboardLayout exposing (Key, Keyboard, keyboardFromModel)

import Dict exposing (Dict)
import HSLuv exposing (HSLuv)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)
import Maybe exposing (andThen)
import Model exposing (Model)

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


getOrBlank : Dict String String -> String -> String
getOrBlank mapping char =
    case Dict.get char mapping of
        Just note ->
            note

        Nothing ->
            ""


prependToRow : Model -> Int -> String -> Keyboard -> Keyboard
prependToRow m rowNum nextChar current =
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
            Dict.get nextChar m.keyMapping
            |> andThen (\name -> Dict.get name m.noteConfig)
            
    in
    let
        color = if Set.member nextChar m.keysPressed then (0.0, 0.0, 100.0) else 
            case note of
                Just n ->
                    n.color

                Nothing ->
                    (0.0, 0.0, 0.0)
    in
    { char = nextChar, note = getOrBlank m.keyMapping nextChar, row = rowNum + 1, colStart = start, colEnd = start + width, color = color } :: current


stringToRow : Model -> Int -> String -> Keyboard
stringToRow m rowNum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.foldl (prependToRow m rowNum) []


keyboardFromModel : Model -> Keyboard
keyboardFromModel m =
    let
        lines =
            String.lines m.keyLayout
    in
    List.indexedMap (stringToRow m) lines
        |> List.concat
