module Game.Initialize exposing (..)

import Model
import Constants
import Game.Generators exposing (generateGame)
import Messages exposing (Msg)
import Random


init : (Maybe Model.GameState, Cmd Msg)
init =
    let
        cmd = Random.generate Messages.NewGame generateGame
    in
        (Nothing, cmd)
    

