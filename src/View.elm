module View exposing (..)

import Vector exposing (Vector)
import Model
import Messages
import Html exposing (..)
import Html.Attributes exposing (..)
import Constants
import Physical

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
        visualAngle = Vector.rotate (turns 0.25) angle
        transform = mergeTransforms [translate visualOffset, rotate visualAngle]
    in
        div
            [ style "font-famly" "sans-serif"
            , style "color" "white"
            , style "width" "32px"
            , style "height" "40px"
            , style "background" "url('/images/ship.png') no-repeat 0 -142px"
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
            , style "color" "white"
            , style "position" "absolute"
            , style "transform" transform
            ]
            [ text "" ]

viewMissile : Model.Missile -> Html Messages.Msg
viewMissile { position, angle } =
    let
        halfWidth = toFloat Constants.missileRadius
        cornerOffset =
            Vector.Cartesian
                { x = halfWidth
                , y = halfWidth
                }
        offset =
            Vector.subtract
                (gameToSpatial position)
                cornerOffset
        transform = mergeTransforms [translate offset, rotate angle]
    in
        div
            [ style "transform" transform
            , style "color" "white"
            , style "position" "absolute"
            , style "width" <| String.fromInt <| Constants.missileRadius * 2
            , style "height" <| String.fromInt <| Constants.missileRadius * 2
            ]
            [ text "." ]
    
    

viewGameState : Model.GameState -> Html Messages.Msg
viewGameState { player, asteroids, missiles } =
    let
        ships =
            duplicateIfNearEdge player
        asteroids_ =
            List.concatMap duplicateIfNearEdge asteroids
        missiles_ =
            List.concatMap duplicateIfNearEdge missiles
    in
        div [ style "position" "relative"
            , style "width" (String.fromInt Constants.worldWidth ++ "px")
            , style "height" (String.fromInt Constants.worldHeight ++ "px")
            , style "background-color" "black"
            , style "overflow" "hidden"
            ]
            [ div []
                (List.map viewShip ships)
            , div []
                (List.map viewAsteroid asteroids_)
            , div []
                (List.map viewMissile missiles_)
            ]


viewGame : Maybe Model.GameState -> Html Messages.Msg
viewGame maybeGame =
    case maybeGame of
        Just game ->
            viewGameState game
        Nothing ->
            text "Loading"


duplicateIfNearEdge : Physical.HasPosition (Physical.HasSize a) -> List (Physical.HasPosition (Physical.HasSize a))
duplicateIfNearEdge obj =
    [obj] ++ duplicateHoriz obj ++ duplicateVert obj


duplicateHoriz : Physical.HasPosition (Physical.HasSize a) -> List (Physical.HasPosition (Physical.HasSize a))
duplicateHoriz obj =
    let
        (x, y) = Vector.toXY obj.position
        buffer = obj.radius * 2
        width = toFloat Constants.worldWidth
    in
        if x < buffer
            then [ {obj | position = Vector.Cartesian { x = x + width, y = y } }]
            else
                if x > (width - buffer)
                then [ {obj | position = Vector.Cartesian { x = x - width, y = y } }]
                else []


duplicateVert : Physical.HasPosition (Physical.HasSize a) -> List (Physical.HasPosition (Physical.HasSize a))
duplicateVert obj =
    let
        (x, y) = Vector.toXY obj.position
        buffer = obj.radius * 2
        height = toFloat Constants.worldHeight
    in
        if y < buffer
            then [ {obj | position = Vector.Cartesian { x = x, y = y + height } }]
            else
                if y > (height - buffer)
                then [ {obj | position = Vector.Cartesian { x = x, y = y - height } }]
                else []