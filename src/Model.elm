module Model exposing (..)

import Constants
import Vector exposing (Vector)
import Physical exposing (Physical)

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
    }

newShip : Vector -> Ship
newShip pos =
    { position = pos
    , velocity = Vector.zero
    , angle = Vector.unit 0
    , angularSpeed = 0
    , mass = Constants.shipMass
    , radius = Constants.shipSize
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

type alias GameState =
    { player: Ship
    , asteroids: List Asteroid
    }
