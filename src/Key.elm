module Key exposing (Key, Keyboard, Row, toKeyboard)

import Dict exposing (Dict)
import HSLuv exposing (HSLuv)
import Maybe exposing (andThen)
import Model exposing (Model)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)


type alias Key =
    { char : String
    , note : String
    , row : Int
    , width : Int
    , color : ( Float, Float, Float )
    }


type alias Row =
    List Key

type alias Keyboard =
    List Row

getOrBlank : Dict String String -> String -> String
getOrBlank mapping char =
    case Dict.get char mapping of
        Just note ->
            note

        Nothing ->
            ""


width char =
    if char == " " then
        1

    else
        2


toKey : Model -> Int -> String -> Key
toKey m rownum nextChar =
    let
        note =
            Dict.get nextChar m.keyMapping
                |> andThen (\name -> Dict.get name m.noteConfig)
    in
    let
        color =
            if Set.member nextChar m.keysPressed then
                ( 0.0, 0.0, 100.0 )

            else
                case note of
                    Just n ->
                        n.color

                    Nothing ->
                        ( 0.0, 0.0, 0.0 )
    in
    { char = nextChar
    , note = getOrBlank m.keyMapping nextChar
    , row = rownum + 1
    , width = width nextChar
    , color = color
    }


toRow : Model -> Int -> String -> Row
toRow m rownum chars =
    String.toList chars
        |> List.map String.fromChar
        |> List.map (toKey m rownum)


toKeyboard : Model -> List Row
toKeyboard m =
    String.lines m.keyLayout
        |> List.indexedMap (toRow m)
