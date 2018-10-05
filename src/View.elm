module View exposing (..)

import Vector exposing (Vector)
import Model
import Messages
import Html exposing (..)
import Html.Attributes exposing (..)
import Constants

gameToSpatial : Vector -> Vector
gameToSpatial =
    identity

translate: Vector -> String
translate translation =
    let
        (x, y) = Vector.toXY translation
    in
        "translate(" ++ String.fromFloat x ++ "px, " ++ String.fromFloat y ++ "px)"

rotate: Vector -> String
rotate rotation =
    let
        t = Vector.angle rotation
    in
        "rotate(" ++ String.fromFloat t ++ "rad)"

scale: Float -> String
scale factor =
    let
        factorStr = String.fromFloat factor
    in
        "scale(" ++ factorStr ++ "," ++ factorStr ++ ")"


mergeTransforms : List String -> String
mergeTransforms transforms =
    String.join " " transforms


viewShip : Model.Ship -> Html Messages.Msg
viewShip { position, angle } =
    let
        cornerOffset = Vector.Cartesian { x = Constants.shipSize, y = Constants.shipSize }
        visualOffset =
            Vector.subtract
                (gameToSpatial position)
                cornerOffset
        visualAngle = Vector.rotate (turns 0.75) angle
        transform = mergeTransforms [translate visualOffset, rotate visualAngle]
    in
        div
            [ style "font-famly" "sans-serif"
            , style "color" "black"
            , style "width" "32px"
            , style "height" "24px"
            , style "background" "url('/images/ship.png') no-repeat 0 -16px"
            , style "background-size" "192px 608px"
            , style "position" "absolute"
            , style "transform" transform
            ]
            [ text "" ]


asteroidSizeToPixels : Model.AsteroidSize -> Int
asteroidSizeToPixels size =
    case size of
        Model.AsteroidSizeXSmall -> 16
        Model.AsteroidSizeSmall  -> 16
        Model.AsteroidSizeMedium -> 32
        Model.AsteroidSizeLarge  -> 48

asteroidSpriteOffset : Model.AsteroidSize -> Int
asteroidSpriteOffset size =
    case size of
        Model.AsteroidSizeXSmall -> 0
        Model.AsteroidSizeSmall  -> 16
        Model.AsteroidSizeMedium -> 32
        Model.AsteroidSizeLarge  -> 64

viewAsteroid : Model.Asteroid -> Html Messages.Msg
viewAsteroid { position, angle, size } =
    let
        pixelSize = asteroidSizeToPixels size
        spriteOffset = asteroidSpriteOffset size
        halfWidth = pixelSize // 2
        cornerOffset = Vector.Cartesian { x = (toFloat halfWidth), y = (toFloat halfWidth)}
        offset =
            Vector.subtract
                (gameToSpatial position)
                cornerOffset
        transform = mergeTransforms [translate offset, rotate angle]
    in
        div
            [ style "font-famly" "sans-serif"
            , style "width" (String.fromInt pixelSize ++ "px")
            , style "height" (String.fromInt pixelSize ++ "px")
            , style "background" ("url('/images/asteroids.png') no-repeat -" ++ String.fromInt spriteOffset ++ "px 0")
            , style "color" "black"
            , style "position" "absolute"
            , style "transform" <| transform
            ]
            [ text "" ]
    

viewGameState : Model.GameState -> Html Messages.Msg
viewGameState { player, asteroids } =
    div [ style "position" "relative" ]
        [ viewShip player
        , div []
            (List.map viewAsteroid asteroids)
        ]

viewGame : Maybe Model.GameState -> Html Messages.Msg
viewGame maybeGame =
    case maybeGame of
        Just game ->
            viewGameState game
        Nothing ->
            text "Loading"