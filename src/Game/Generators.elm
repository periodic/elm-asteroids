module Game.Generators exposing (..)

import Model
import Random exposing (Generator)
import Vector exposing (Vector)
import Constants

makePolar r t = Vector.Polar { r = r, t = t }
makeCartesian x y = Vector.Cartesian { x = x, y = y }

generateAngle : Generator Float
generateAngle =
    Random.float 0 (turns 1)

generateWorldVector : Generator Vector
generateWorldVector =
    Random.map2
        makeCartesian
        (Random.float 0 (toFloat Constants.worldWidth))
        (Random.float 0 (toFloat Constants.worldHeight))

generateAsteroidSize : Generator Model.AsteroidSize
generateAsteroidSize =
    Random.uniform
        Model.AsteroidSizeXSmall
        [ Model.AsteroidSizeSmall
        , Model.AsteroidSizeMedium
        , Model.AsteroidSizeLarge
        ]

{- 
 - Generates the asteroid position within a given band. Does not place things
 - in corners or within some range of the player. Does not distribute uniformly.
 -}
generateAsteroidPosition : Generator Vector
generateAsteroidPosition = 
    let
        minAsteroidDistance =
            toFloat Constants.worldHeight / 2 - Constants.newWorldBufferRadius
        maxAsteroidDistance =
            toFloat Constants.worldHeight / 2
        generateR =
            Random.float minAsteroidDistance maxAsteroidDistance
    in
        Random.map2 makePolar generateR generateAngle
            |> Random.map (Vector.add Constants.worldCenter)

generateAsteroidVelocity : Generator Vector
generateAsteroidVelocity =
    let
        generateMagnitude =
            Random.float Constants.minAsteroidSpeed Constants.maxAsteroidSpeed
    in
        Random.map2 makePolar generateMagnitude generateAngle
    

generateAsteroid : Generator Model.Asteroid
generateAsteroid =
    Random.map3
        Model.newAsteroid
        generateAsteroidSize
        generateAsteroidPosition
        generateAsteroidVelocity

generateAsteroids : Generator (List Model.Asteroid)
generateAsteroids =
    Random.int Constants.minAsteroids Constants.maxAsteroids
        |> Random.andThen (\num -> Random.list num generateAsteroid)

generateGame : Random.Generator Model.GameState
generateGame =
    Random.map
        (\asteroids ->
            { player = Model.newShip Constants.worldCenter
            , asteroids = asteroids
            , missiles = []
            })
        generateAsteroids
        

    