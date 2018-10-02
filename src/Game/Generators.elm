module Game.Generators exposing (..)

import Model
import Random exposing (Generator)
import Vector exposing (Vector)
import Constants

generateWorldVector : Generator Vector
generateWorldVector =
    Random.map2
        Vector.Cartesian
        (Random.float 0 (toFloat Constants.worldWidth))
        (Random.float 0 (toFloat Constants.worldHeight))

generateAsteroidSize : Generator Float
generateAsteroidSize =
    Random.float Constants.minAsteroidSize Constants.maxAsteroidSize

generateAsteroid : Generator Model.Asteroid
generateAsteroid =
    Random.map2
        Model.newAsteroid
        generateAsteroidSize
        generateWorldVector

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
            })
        generateAsteroids
        

    