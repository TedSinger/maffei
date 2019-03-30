module Configs exposing (myKeys, myLayout, myNoteCfg, myKeyboardMapping)

import Dict exposing (Dict)
import Json.Encode as E
import Notes exposing (genNoteCfg, getName, Note, NoteConfig)


myNoteCfg : NoteConfig
myNoteCfg =
    List.range -24 36
        |> List.map genNoteCfg
        |> Dict.fromList


myKeys =
    "zxcvbasdfgqwert123456nm,.hjkl;yuiop[]7890-="


myKeyboardMapping =
    List.range 0 45
        |> List.map (\i -> ( String.slice i (i + 1) myKeys, getName (i - 14) ))
        |> Dict.fromList


myLayout =
    "123456 7890-=\n qwert yuiop[]\n  asdfg hjkl;\n   zxcvb nm,."
