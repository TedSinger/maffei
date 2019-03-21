port module KeyboardState exposing (sendActiveNotes, sendNoteCfg)

import Json.Encode as E


port sendList : E.Value -> Cmd msg


sendActiveNotes v =
    E.list E.string v |> sendList


port sendNoteCfg : E.Value -> Cmd msg
