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

        Model.FireMissile ->
            handleFireMissile state
        
        Model.NoCommand ->
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

handleFireMissile : GameState -> GameState
handleFireMissile state =
    let
        player = state.player
    in
        if player.loadedMissiles > 0
            then fireMissile state
            else state

fireMissile : GameState -> GameState
fireMissile state =
    let
        player = state.player
        missileDistanceFromPlayer = player.radius + Constants.missileRadius
        missileOffset = Vector.scale missileDistanceFromPlayer player.angle
        missilePosition =
            Vector.add
                player.position
                missileOffset
        missile = Model.newMissile missilePosition player.velocity player.angle
        player_ =
            { player
            | loadedMissiles = player.loadedMissiles - 1
            }
    in
        { state | player = player_, missiles = missile :: state.missiles }
    

handleTick : Float -> GameState -> GameState
handleTick ms state =
    let
        deltaT = ms / 1000
    in
        state
            |> updatePlayer deltaT
            |> updateAsteroids deltaT
            |> updateMissiles deltaT
            |> handleCollisions


updatePlayer : Float -> GameState -> GameState
updatePlayer deltaT state =
    let
        player_ = state.player 
                |> Physical.updatePosition deltaT
                |> Physical.clampPosition
                |> updateShipVelocity deltaT
                |> updateAmmo deltaT
    in
        { state | player = player_ }

updateAmmo : Float -> Model.Ship -> Model.Ship
updateAmmo deltaT ship =
    if ship.loadedMissiles >= Constants.maximumMissiles
        then ship
        else
            if ship.missileReload >= Constants.missileReloadPeriod
                then loadMissile ship
                else { ship | missileReload = Debug.log "Reloading..." <| ship.missileReload + deltaT }

loadMissile : Model.Ship -> Model.Ship
loadMissile ship =
    { ship
    | loadedMissiles = Debug.log "Loaded missile" <| ship.loadedMissiles + 1 
    , missileReload = 0
    }
    

updateAsteroids : Float -> GameState -> GameState
updateAsteroids deltaT state =
    let
        asteroids_ =
            List.map
                (Physical.updatePosition deltaT >> Physical.clampPosition)
                state.asteroids 
    in
        { state | asteroids = asteroids_ }

updateMissiles : Float -> GameState -> GameState
updateMissiles deltaT state =
    let
        missiles_ =
            List.map
                (Physical.updatePosition deltaT >> Physical.clampPosition)
                state.missiles 
    in
        { state | missiles = missiles_ }

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
                |> Vector.rotate (Vector.angle ship.angle)

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
        handleMissileCollisions
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

handleMissileCollisions : GameState -> GameState
handleMissileCollisions state =
    let
        missiles = state.missiles
        asteroids = state.asteroids
    in
        List.foldr
            handleMissileCollisionsFold
            { state | missiles = [] }
            missiles

handleMissileCollisionsFold : Model.Missile -> GameState -> GameState
handleMissileCollisionsFold missile state =
    case checkMissileCollisions missile state.asteroids of
        Just asteroids_ ->
            { state | asteroids = asteroids_ }
        Nothing ->
            { state | missiles = missile :: state.missiles }


checkMissileCollisions : Model.Missile -> List Model.Asteroid -> Maybe (List Model.Asteroid)
checkMissileCollisions missile asteroids =
    case asteroids of
        [] ->
            Nothing
        asteroid :: remaining ->
            if Physical.overlaps missile asteroid
                then
                    Just <| splitAsteroid missile.velocity asteroid ++ remaining
                else
                    checkMissileCollisions missile remaining |> Maybe.map (\asters -> asteroid :: asters)

nextSmallerAsteroid : Model.AsteroidSize -> Maybe Model.AsteroidSize
nextSmallerAsteroid size =
    case size of
        Model.AsteroidSizeXSmall ->
            Nothing
        Model.AsteroidSizeSmall ->
            Just Model.AsteroidSizeXSmall
        Model.AsteroidSizeMedium ->
            Just Model.AsteroidSizeSmall
        Model.AsteroidSizeLarge ->
            Just Model.AsteroidSizeMedium

splitAsteroid : Vector -> Model.Asteroid -> List Model.Asteroid
splitAsteroid impactDirection asteroid =
    case nextSmallerAsteroid asteroid.size of
        Nothing ->
            []
        Just newSize ->
            let
                baseVelocity = asteroid.velocity
                normalDirection = Vector.toUnit impactDirection

                relativeVelocity =
                    Vector.scale Constants.asteroidBreakupSpeed normalDirection
                relativeVelocity1 =
                    Vector.rotate (turns 0.25) relativeVelocity
                relativeVelocity2 =
                    Vector.rotate (turns 0.75) relativeVelocity

                offset =
                    Vector.scale (toFloat (Model.asteroidSizeToRadius asteroid.size) / 2) normalDirection
                offset1 =
                    Vector.rotate (turns 0.25) offset
                offset2 =
                    Vector.rotate (turns 0.75) offset
                
                position1 =
                    Vector.add asteroid.position offset1
                velocity1 =
                    Vector.add asteroid.velocity relativeVelocity1
                position2 =
                    Vector.add asteroid.position offset2
                velocity2 =
                    Vector.add asteroid.velocity relativeVelocity2
            in
                [ Model.newAsteroid newSize position1 velocity1
                , Model.newAsteroid newSize position2 velocity2
                ]
    