module Model exposing (Model, UIMode(..), withKeyChange)

import Dict exposing (Dict)
import Notes exposing (Note, NoteConfig)
import Set exposing (Set)


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


translateKeyPresses : Dict String String -> String -> Set String -> List String
translateKeyPresses keyboardMapping layout keys =
    Set.toList keys
        |> List.filter (\k -> String.contains k layout)
        |> List.filterMap (\k -> Dict.get k keyboardMapping)


withKeyChange : Model -> Bool -> String -> ( Model, List String )
withKeyChange oldModel active char =
    let
        newKeysPressed =
            if active then
                Set.insert char oldModel.keysPressed

            else
                Set.remove char oldModel.keysPressed
    in
    let
        newNotesPressed =
            translateKeyPresses oldModel.keyMapping oldModel.keyLayout newKeysPressed
    in
    ( { oldModel | keysPressed = newKeysPressed }
    , newNotesPressed
    )
