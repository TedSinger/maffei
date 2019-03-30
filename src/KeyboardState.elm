port module KeyboardState exposing (sendActiveNotes, sendNoteConfig)

import Dict exposing (Dict)
import Json.Encode as E
import Notes exposing (Note, NoteConfig)


port sendList : E.Value -> Cmd msg


sendActiveNotes v =
    E.list E.string v |> sendList


noteToDict name note =
    Dict.fromList [ ( "freq", note.freq ), ( "gain", note.gain ) ]


sendNoteConfig : NoteConfig -> Cmd msg
sendNoteConfig nc =
    Dict.map noteToDict nc
        |> E.dict identity (E.dict identity E.float)
        |> sendNoteCfg


port sendNoteCfg : E.Value -> Cmd msg
