port module KeyboardState exposing (activeNotes, noteCfg)

import Json.Encode as E

port activeNotes : E.Value -> Cmd msg

port noteCfg : E.Value -> Cmd msg