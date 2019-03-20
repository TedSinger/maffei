module KeyboardLayout exposing (Key, KeyPlacement, Keyboard, keyboardFromModel)

import Dict exposing (Dict)


type alias KeyPlacement =
    { row : Int, colStart : Int, colEnd : Int }


type alias Key =
    { char : String, note : String }


type alias Keyboard =
    List ( Key, KeyPlacement )


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
            { char = nextChar, note = getOrBlank noteDict nextChar }
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
                ( headKey, headPlace ) :: tail ->
                    headPlace.colEnd

                [] ->
                    1
    in
    ( key, { row = rowNum, colStart = start, colEnd = start + width } ) :: current


stringToRow : Dict String String -> Int -> String -> Keyboard
stringToRow noteDict rowNum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.foldl (prependToRow noteDict rowNum) []
        |> List.reverse


keyboardFromModel : Dict String String -> String -> Keyboard
keyboardFromModel noteDict chars =
    let
        lines =
            String.lines chars
    in
    List.indexedMap (stringToRow noteDict) lines
        |> List.concat
