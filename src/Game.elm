module Game exposing (..)

import Constants
import Vector exposing (Vector)
import Messages exposing (Msg)
import Model exposing (GameState)
import Physical
import Random

import Debug

update : Msg -> GameState -> GameState
update msg state =
    case msg of
        Messages.Tick delta ->
            handleTick delta state

        Messages.ShipCommand command ->
            updateForCommandStart command state


updateForCommandStart : Model.ShipCommand -> GameState -> GameState
updateForCommandStart command state =
    case command of
        Model.ActivateThruster thruster ->
            { state | player = turnThrusterOn thruster state.player }

        Model.DeactivateThruster thruster ->
            { state | player = turnThrusterOff thruster state.player }

        _ ->
            state

updateThrusters : (Model.Thrusters -> Model.Thrusters) -> Model.Ship -> Model.Ship
updateThrusters updateFn ship =
    { ship | thrusters = updateFn ship.thrusters }

updateThruster : (Bool -> Bool) -> Model.Thruster -> Model.Ship -> Model.Ship
updateThruster f thruster ship =
    case thruster of
        Model.Forward ->
            updateThrusters
                (\thrusters -> { thrusters | forward = f ship.thrusters.forward })
                ship
        Model.Back ->
            updateThrusters
                (\thrusters -> { thrusters | back = f ship.thrusters.back })
                ship
        Model.Right ->
            updateThrusters
                (\thrusters -> { thrusters | right = f ship.thrusters.right })
                ship
        Model.Left ->
            updateThrusters
                (\thrusters -> { thrusters | left = f ship.thrusters.left })
                ship


turnThrusterOn : Model.Thruster -> Model.Ship -> Model.Ship
turnThrusterOn =
    updateThruster (\_ -> True)

turnThrusterOff : Model.Thruster -> Model.Ship -> Model.Ship
turnThrusterOff =
    updateThruster (\_ -> False)

updatePlayer : (Model.Ship -> Model.Ship) -> GameState -> GameState
updatePlayer f state =
    { state | player = f state.player }

handleTick : Float -> GameState -> GameState
handleTick ms state =
    let
        deltaT = ms / 1000
    in
        state
            |> updatePlayer (
                Physical.updatePosition deltaT
                    >> clampPosition
                    >> updateShipVelocity deltaT)

updateShipVelocity : Float -> Model.Ship -> Model.Ship
updateShipVelocity deltaT ship =
    let
        forwardForce =
            if ship.thrusters.back
                then Constants.forwardThrustVec
                else Vector.Polar 0 0
        backForce =
            if ship.thrusters.forward
                then Constants.backThrustVec
                else Vector.Polar 0 0
            
        totalForce =
            Vector.add forwardForce backForce
                |> Vector.rotateTo ship.angle

        -- Rotational thrusters are impulse thrusters, so turn rotation on or off.
        rightForce = 
            if ship.thrusters.left
                then Constants.rotationalThrust
                else 0
        leftForce = 
            if ship.thrusters.right
                then Constants.rotationalThrust
                else 0

        totalAngularForce =
            (rightForce - leftForce)
    in
        ship
            |> Physical.applyForce deltaT totalForce
            |> Physical.setAngularSpeed totalAngularForce



modFloatBy : Int -> Float -> Float
modFloatBy base n =
    if n < 0
        then
            modFloatBy base (n + toFloat base)
        else
            if n > toFloat base
                then modFloatBy base (n - toFloat base)
                else n


clampPosition : Model.Ship -> Model.Ship
clampPosition ship =
    let
        (x,y) = Vector.toXY ship.position
        x_ = modFloatBy Constants.worldWidth x
        y_ = modFloatBy Constants.worldWidth y
        position_ = Vector.Cartesian x_ y_
    in
        { ship | position = position_ }