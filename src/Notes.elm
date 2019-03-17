module Notes exposing (genNoteCfg, getName)

import Dict exposing (Dict)
import EqualLoudness exposing (fletcherMunson, linearInterpolate)
import Set


getName : Int -> String
getName halfStepsFromA440 =
    let
        rem =
            remainderBy 12 (halfStepsFromA440 + 120)
    in
    let
        letters =
            Dict.fromList
                [ ( 0, "A" )
                , ( 1, "A#" )
                , ( 2, "B" )
                , ( 3, "C" )
                , ( 4, "C#" )
                , ( 5, "D" )
                , ( 6, "D#" )
                , ( 7, "E" )
                , ( 8, "F" )
                , ( 9, "F#" )
                , ( 10, "G" )
                , ( 11, "G#" )
                ]
    in
    let
        octave =
            (halfStepsFromA440 + 57) // 12
    in
    case Dict.get rem letters of
        Just letter ->
            String.concat [ letter, String.fromInt octave ]

        Nothing ->
            "?"


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
    ( getName halfStepsFromA440
    , Dict.fromList
        [ ( "freq", freq )
        , ( "gain", gain )
        , ( "halfStepsFromA440", toFloat halfStepsFromA440 )
        ]
    )
