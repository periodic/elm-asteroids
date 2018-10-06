module Game exposing (..)

import Constants
import Vector exposing (Vector)
import Messages exposing (Msg)
import Model exposing (GameState)
import Physical exposing (Physical)
import Random

import Debug

update : Msg -> GameState -> GameState
update msg state =
    case msg of
        Messages.Tick delta ->
            handleTick delta state

        Messages.ShipCommand command ->
            updateForCommandStart command state
        
        _ ->
            state


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

handleTick : Float -> GameState -> GameState
handleTick ms state =
    let
        deltaT = ms / 1000
    in
        state |> updatePlayer deltaT |> updateAsteroids deltaT |> handleCollisions

updatePlayer : Float -> GameState -> GameState
updatePlayer deltaT state =
    let
        player_ = state.player 
                |> Physical.updatePosition deltaT
                |> Physical.clampPosition
                |> updateShipVelocity deltaT
    in
        { state | player = player_ }

updateAsteroids : Float -> GameState -> GameState
updateAsteroids deltaT state =
    let
        asteroids_ =
            List.map
                (Physical.updatePosition deltaT >> Physical.clampPosition)
                state.asteroids 
    in
        { state | asteroids = asteroids_ }

updateShipVelocity : Float -> Model.Ship -> Model.Ship
updateShipVelocity deltaT ship =
    let
        forwardForce =
            if ship.thrusters.back
                then Constants.forwardThrustVec
                else Vector.zero
        backForce =
            if ship.thrusters.forward
                then Constants.backThrustVec
                else Vector.zero
            
        totalForce =
            Vector.add forwardForce backForce
                |> Vector.rotateTo ship.angle

        -- Rotational thrusters are impulse thrusters, so turn rotation on or off.
        rightForce = 
            if ship.thrusters.left
                then Constants.rotationalSpeed
                else 0
        leftForce = 
            if ship.thrusters.right
                then Constants.rotationalSpeed
                else 0

        totalAngularForce =
            (leftForce - rightForce)
    in
        ship
            |> Physical.applyForce deltaT totalForce
            |> Physical.setAngularSpeed totalAngularForce

handleCollisions : Model.GameState -> Model.GameState
handleCollisions state =
    let
        (player_, asteroids_) = handleShipCollisions state.player state.asteroids
        asteroids__ = handleInterAsteroidCollisions asteroids_
    in
        { state | player = player_, asteroids = asteroids__}
    

handleShipCollisions : Model.Ship -> List Model.Asteroid -> (Model.Ship, List Model.Asteroid)
handleShipCollisions ship asteroids =
    List.foldr handleCollisionsFold (ship, []) asteroids

handleCollisionsFold : Physical a -> (Physical b, List (Physical a)) -> (Physical b, List (Physical a))
handleCollisionsFold asteroid (ship, asteroids) =
    let
        overlap = Physical.overlap asteroid ship
    in
        case overlap of
            Just shipNormal ->
                let
                    (ship_, asteroid_) =
                        Physical.collide ship asteroid
                in
                    (ship_, asteroid_ :: asteroids)
            Nothing ->
                (ship, asteroid :: asteroids)

handleInterAsteroidCollisions : List Model.Asteroid -> List Model.Asteroid
handleInterAsteroidCollisions asteroids =
    case asteroids of
        [] ->
            []
        asteroid :: otherAsteroids ->
            let
                (asteroid_, otherAsteroids_) = List.foldr handleCollisionsFold (asteroid, []) otherAsteroids
            in
                asteroid_ :: handleInterAsteroidCollisions otherAsteroids_
