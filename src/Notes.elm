module Notes exposing (genNoteCfg, getName)

import Dict exposing (Dict)
import EqualLoudness exposing (fletcherMunson, linearInterpolate)
import Set


getNoteLetter : Int -> String
getNoteLetter stepsFromA =
    let
        rem =
            remainderBy 12 (stepsFromA + 120)
    in
    let
        notes =
            Dict.fromList
                [ ( 0, "A" )
                , ( 1, "A" )
                , ( 2, "B" )
                , ( 3, "C" )
                , ( 4, "C" )
                , ( 5, "D" )
                , ( 6, "D" )
                , ( 7, "E" )
                , ( 8, "F" )
                , ( 9, "F" )
                , ( 10, "G" )
                , ( 11, "G" )
                ]
    in
    case Dict.get rem notes of
        Just letter ->
            letter

        Nothing ->
            "?"


getAccidental : Int -> String
getAccidental stepsFromA =
    let
        rem =
            remainderBy 12 (stepsFromA + 120)
    in
    let
        sharps =
            Set.fromList [ 1, 4, 6, 9, 11 ]
    in
    if Set.member rem sharps then
        "#"

    else
        ""


getName : Int -> String
getName halfStepsFromA440 =
    let
        letter =
            getNoteLetter halfStepsFromA440
    in
    let
        accidental =
            getAccidental halfStepsFromA440
    in
    let
        octave =
            (halfStepsFromA440 + 57) // 12
    in
    String.concat [ letter, accidental, String.fromInt octave ]


genNoteCfg : Int -> ( String, Dict String Float )
genNoteCfg halfStepsFromA440 =
    let
        freq =
            440 * 2 ^ (toFloat halfStepsFromA440 / 12)
    in
    let
        gain =
            linearInterpolate freq fletcherMunson
    in
    ( getName halfStepsFromA440, Dict.fromList [ ( "freq", freq ), ( "gain", gain ), ( "halfStepsFromA440", toFloat halfStepsFromA440 ) ] )
