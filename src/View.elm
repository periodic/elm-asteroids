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
        (_, t) = Vector.toPolar (Vector.rotate (turns 0.25) rotation)
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
        offset = gameToSpatial position
        transform = mergeTransforms [translate offset, rotate angle]
    in
        div
            [ style "font-famly" "sans-serif"
            , style "color" "black"
            , style "position" "absolute"
            , style "transform" transform
            ]
            [ text "V" ]


viewAsteroid : Model.Asteroid -> Html Messages.Msg
viewAsteroid { position, angle, size } =
    let
        offset = gameToSpatial position
        transform = mergeTransforms [translate offset, rotate angle, scale size]
    in
        div
            [ style "font-famly" "sans-serif"
            , style "color" "black"
            , style "position" "absolute"
            , style "transform" <| transform
            ]
            [ text "O" ]
    

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