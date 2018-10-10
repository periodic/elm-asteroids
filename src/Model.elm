module Model exposing (..)

import Constants
import Vector exposing (Vector)
import Physical exposing (..)

import Random

type Thruster
    = Forward
    | Back
    | Left
    | Right

type ShipCommand
    = ActivateThruster Thruster
    | DeactivateThruster Thruster
    | FireMissile
    | NoCommand

type alias Weight = Float

type alias Thrusters =
    { left: Bool
    , right: Bool
    , forward: Bool
    , back: Bool
    }

type alias Ship =
    Physical
    { thrusters: Thrusters
    , loadedMissiles: Int
    , missileReload: Float -- ms
    }

newShip : Vector -> Ship
newShip pos =
    { position = pos
    , velocity = Vector.zero
    , angle = Vector.unit 0
    , angularSpeed = 0
    , mass = Constants.shipMass
    , radius = Constants.shipSize
    , loadedMissiles = 1
    , missileReload = 0
    , thrusters = {
        left = False,
        right = False,
        forward = False,
        back = False
    }
    }

type alias Asteroid =
    Physical {
        size : AsteroidSize
    }

type AsteroidSize
    = AsteroidSizeXSmall
    | AsteroidSizeSmall
    | AsteroidSizeMedium
    | AsteroidSizeLarge

asteroidSizeToRadius : AsteroidSize -> Int
asteroidSizeToRadius size =
    case size of
        AsteroidSizeXSmall -> Constants.asteroidSize.xsmall
        AsteroidSizeSmall -> Constants.asteroidSize.small
        AsteroidSizeMedium -> Constants.asteroidSize.medium
        AsteroidSizeLarge -> Constants.asteroidSize.large

newAsteroid : AsteroidSize -> Vector -> Vector -> Asteroid
newAsteroid size pos velocity =
    let
        radius = toFloat <| asteroidSizeToRadius size
    in
        { position = pos
        , velocity = velocity
        , angle = Vector.unit 0
        , angularSpeed = 0
        , size = size
        , radius = radius
        , mass = radius * radius -- For now just make size and mass scale quadratically.
        }

type alias Missile =
    HasPosition (HasVelocity (HasSize {}))

newMissile : Vector -> Vector -> Vector -> Missile
newMissile position baseVelocity direction =
    let
        angle =
            Vector.toUnit direction
        relativeVelocity =
            Vector.scale Constants.missileSpeed angle
        absoluteVelocity =
            Vector.add
                baseVelocity
                relativeVelocity
    in
        { position = position
        , angle = angle
        , velocity = absoluteVelocity
        , angularSpeed = 0
        , radius = Constants.missileRadius
        }
    

type alias GameState =
    { player: Ship
    , asteroids: List Asteroid
    , missiles: List Missile
    }
