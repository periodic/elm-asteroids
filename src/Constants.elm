module Constants exposing (..)

import Vector exposing (Vector)

worldWidth : Int
worldWidth = 400

worldHeight : Int
worldHeight = 400

worldCenter : Vector
worldCenter =
    Vector.Cartesian (toFloat worldWidth / 2) (toFloat worldHeight / 2)

-- kg-rad/s
rotationalThrust : Float
rotationalThrust = 5

-- kg-m/s^2
forwardThrust : Float
forwardThrust = 80

forwardThrustVec =
    Vector.Polar forwardThrust 0

-- kg-m/s^2
backThrust : Float
backThrust = 50

backThrustVec =
    Vector.Polar backThrust (turns 0.5)

-- m/s
maxSpeed : Int
maxSpeed = 200

minAsteroids = 1
maxAsteroids = 5

minAsteroidSize = 0.5
maxAsteroidSize = 4