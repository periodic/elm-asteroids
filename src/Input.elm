module Input exposing (decodeKeyDown, decodeKeyUp)

import Json.Decode as Decode exposing (Decoder)
import Messages exposing (Msg)
import Model

orElse : Maybe a -> Maybe a -> Maybe a
orElse default m =
    case m of
        Nothing ->
            default
        just ->
            just


decodeKeyUp : Decoder Msg
decodeKeyUp = 
    Decode.field "key" Decode.string
        |> Decode.map (\key ->
            keyToDirection key
                |> Maybe.map Model.ActivateThruster
                |> orElse (keyToWeaponsCommand key)
                |> Maybe.withDefault Model.NoCommand)
        |> Decode.map Messages.ShipCommand


decodeKeyDown : Decoder Msg
decodeKeyDown =
    Decode.field "key" Decode.string
        |> Decode.map (\key ->
            keyToDirection key
                |> Maybe.map Model.DeactivateThruster
                |> Maybe.withDefault Model.NoCommand)
        |> Decode.map Messages.ShipCommand

keyToDirection : String -> Maybe Model.Thruster
keyToDirection string =
    case string of
        "ArrowLeft" ->
            Just Model.Left
        "ArrowRight" ->
            Just Model.Right
        "ArrowUp" ->
            Just Model.Back
        "ArrowDown" ->
            Just Model.Forward
        "a" ->
            Just Model.Left
        "d" ->
            Just Model.Right
        "w" ->
            Just Model.Back
        "s" ->
            Just Model.Forward
        _ ->
            Nothing

keyToWeaponsCommand : String -> Maybe Model.ShipCommand
keyToWeaponsCommand string =
    case string of
        " " ->
            Debug.log "Firing missile" <|
            Just Model.FireMissile
        _ ->
            Nothing

