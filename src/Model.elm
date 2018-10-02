module Model exposing (..)

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
    , velocity = Vector.origin
    , angle = Vector.unit 0
    , angularSpeed = 0
    , mass = 1
    , thrusters = {
        left = False,
        right = False,
        forward = False,
        back = False
    }
    }

type alias Asteroid =
    Physical
    { size: Float -- size relative to baseline?
    }

newAsteroid : Float -> Vector -> Asteroid
newAsteroid size pos =
    { position = pos
    , velocity = Vector.origin
    , angle = Vector.unit 0
    , angularSpeed = 0
    , mass = size -- For now just make size and mass scale linearly.
    , size = size
    }

type alias GameState =
    { player: Ship
    , asteroids: List Asteroid
    }

