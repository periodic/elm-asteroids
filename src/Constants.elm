module Constants exposing (..)

import Vector exposing (Vector)

worldWidth : Int
worldWidth = 400

worldHeight : Int
worldHeight = 400

worldCenter : Vector
worldCenter =
    Vector.Cartesian
        { x = toFloat worldWidth / 2
        , y = toFloat worldHeight / 2
        }

newWorldBufferRadius : Float
newWorldBufferRadius = 50 * shipMass

-- kg-rad/s
rotationalSpeed : Float
rotationalSpeed = 5

-- kg-m/s^2
forwardThrust : Float
forwardThrust = 80 * shipMass

forwardThrustVec =
    Vector.Polar { r = forwardThrust, t = 0 }

-- kg-m/s^2
backThrust : Float
backThrust = 50 * shipMass

backThrustVec =
    Vector.Polar { r = backThrust, t = turns 0.5 }

-- m/s
maxSpeed : Int
maxSpeed = 200

shipSize = 16
shipMass = 100

minAsteroids = 2
maxAsteroids = 5

asteroidSize =
    { xsmall = 8
    , small = 16
    , medium = 24
    , large = 32
    }

minAsteroidSpeed = 5
maxAsteroidSpeed = 20

missileRadius = 5
missileSpeed = 50