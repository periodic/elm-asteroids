module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Game.Initialize
import Model
import Messages
import View
import Input
import Game


type alias Model = Maybe Model.GameState
type alias Msg = Messages.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Messages.NewGame game ->
            (Just game, Cmd.none)
        gameMsg ->
            case model of
                Just game ->
                    (Just (Game.update gameMsg game), Cmd.none)
                Nothing ->
                    (Nothing, Cmd.none)

main : Program () Model Msg
main =
    Browser.element
        { init = always Game.Initialize.init
        , view = View.viewGame
        , update = update
        , subscriptions = always
            <| Sub.batch
                [ Browser.Events.onAnimationFrameDelta Messages.Tick
                , Browser.Events.onKeyDown Input.decodeKeyUp
                , Browser.Events.onKeyUp Input.decodeKeyDown
                ]
        }