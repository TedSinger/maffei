module Msg exposing (Msg(..), update)

import Dict exposing (Dict)
import KeyboardState exposing (sendActiveNotes, sendNoteConfig)
import Model exposing (Model, UIMode(..), withKeyChange)
import Set exposing (Set)


type Msg
    = NewLayout String
    | NewNote String String
    | KeyActive Bool String
    | StartPlaying
    | EditLayout


update : Msg -> Model -> ( Model, Cmd msg )
update msg oldModel =
    case msg of
        NewLayout s ->
            ( { oldModel | keyLayout = s }, Cmd.none )

        NewNote char s ->
            ( { oldModel | keyMapping = Dict.insert char s oldModel.keyMapping }, Cmd.none )

        KeyActive active char ->
            if Set.member char oldModel.keysPressed /= active then
                let
                    ( newModel, newNotes ) =
                        withKeyChange oldModel active char
                in
                ( newModel, sendActiveNotes newNotes )

            else
                ( oldModel, Cmd.none )

        StartPlaying ->
            ( { oldModel | uiMode = Playing }, Cmd.none )

        EditLayout ->
            ( { oldModel | uiMode = EditingLayout }, Cmd.none )
