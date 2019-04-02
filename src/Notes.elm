module Notes exposing (genNoteCfg, getName, Note, NoteConfig)

import Dict exposing (Dict)
import EqualLoudness exposing (fletcherMunson, linearInterpolate)
import HSLuv exposing (HSLuv)
import Set


type alias Note =
    { halfStepsFromA440 : Int
    , freq : Float
    , gain : Float
    , name : String
    , color : (Float, Float, Float)
    }


type alias NoteConfig =
    Dict String Note


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


genNoteCfg : Int -> ( String, Note )
genNoteCfg halfStepsFromA440 =
    let
        freq =
            440 * 2 ^ (toFloat halfStepsFromA440 / 12)
    in
    let
        gain =
            linearInterpolate freq fletcherMunson
    in
    let
        name =
            getName halfStepsFromA440
    in
    let hue = (toFloat (remainderBy 12 (halfStepsFromA440 + 120))) * 30 in
    let lightness = (toFloat (50 + halfStepsFromA440)) in 

    ( name
    , { freq = freq, gain = gain, halfStepsFromA440 = halfStepsFromA440, name = name, color = (hue, 100.0, lightness) }
    )
