module Model exposing (Model, UIMode(..))
import Set exposing (Set)
import Dict exposing (Dict)
import Notes exposing (Note, NoteConfig)



type UIMode
    = Playing
    | EditingLayout

type alias Model =
    { keyLayout : String
    , keyMapping : Dict String String
    , keysPressed : Set String
    , noteConfig : NoteConfig
    , uiMode : UIMode
    }
