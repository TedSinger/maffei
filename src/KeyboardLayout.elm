module KeyboardLayout exposing (Key, Keyboard, keyboardFromModel)

import Dict exposing (Dict)
import HSLuv exposing (HSLuv)

type alias Key =
    { char : String
    , note : String
    , row : Int
    , colStart : Int
    , colEnd : Int
    }


type alias Keyboard =
    List Key


getOrBlank : Dict String String -> String -> String
getOrBlank noteDict char =
    case Dict.get char noteDict of
        Just note ->
            note

        Nothing ->
            ""


prependToRow : Dict String String -> Int -> String -> Keyboard -> Keyboard
prependToRow noteDict rowNum nextChar current =
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
    { char = nextChar, note = getOrBlank noteDict nextChar, row = rowNum + 1, colStart = start, colEnd = start + width } :: current


stringToRow : Dict String String -> Int -> String -> Keyboard
stringToRow noteDict rowNum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.foldl (prependToRow noteDict rowNum) []


keyboardFromModel : Dict String String -> String -> Keyboard
keyboardFromModel noteDict chars =
    let
        lines =
            String.lines chars
    in
    List.indexedMap (stringToRow noteDict) lines
        |> List.concat
