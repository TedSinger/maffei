module Configs exposing (myLayout, myKeyMapping, myNoteCfg)

import Dict exposing (Dict)
import Json.Encode as E
import Notes exposing (genNoteCfg, getName, Note, NoteConfig)
import Set exposing (Set)


myNoteCfg : NoteConfig
myNoteCfg =
    List.range -24 36
        |> List.map genNoteCfg
        |> Dict.fromList


myKeys =
    "zxcvbasdfgqwert12345nm,.hjkl;yuiop[]67890-="


myKeyMapping =
    List.range 0 45
        |> List.map (\i -> ( String.slice i (i + 1) myKeys, getName (i - 14) ))
        |> Dict.fromList


myLayout =
    "12345 67890-=\n qwert yuiop[]\n  asdfg hjkl;\n   zxcvb nm,."
