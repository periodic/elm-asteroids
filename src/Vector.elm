module Vector exposing (..)

type Vector
    = Polar { r: Float, t: Float }
    | Cartesian { x: Float, y: Float }


unit : Float -> Vector
unit t =
    Polar { r = 1, t = t }


toUnit : Vector -> Vector
toUnit vec =
    case vec of
        Polar { t } ->
            unit t
        Cartesian { x, y } ->
            let
                (_, t) = Basics.toPolar (x, y)
            in
                unit t


toXY : Vector -> (Float, Float)
toXY vec =
    case vec of
        Polar {r, t} ->
            Basics.fromPolar (r, t)
        Cartesian {x, y} ->
            (x, y)


zero : Vector
zero =
    Cartesian {x = 0, y = 0}


add : Vector -> Vector -> Vector
add v1 v2 = 
    let
        (x1, y1) = toXY v1
        (x2, y2) = toXY v2
    in
        Cartesian { x = (x1 + x2), y = (y1 + y2) }


scale : Float -> Vector -> Vector
scale factor vec =
    case vec of
        Polar {r, t} ->
            Polar { r = (r * factor), t = t }
        Cartesian {x, y} ->
            Cartesian { x = x * factor, y = y * factor }

rotate : Float -> Vector -> Vector
rotate deltaT vec =
    case vec of
        Polar {r, t} ->
            Polar { r = r, t = (t + deltaT) }
        Cartesian {x, y} ->
            let
                (r, t) = Basics.toPolar (x, y)
            in
                Polar { r = r, t = (t + deltaT) }

rotateTo : Vector -> Vector -> Vector
rotateTo toVec vec =
    rotate (angle toVec) vec

angle : Vector -> Float
angle vec =
    case vec of
        Polar { t } ->
            t
        Cartesian {x, y} ->
            toPolar (x, y) |> Tuple.second
    
magnitude : Vector -> Float
magnitude vec =
    case vec of
        Polar { r } ->
            r
        Cartesian {x, y} ->
            toPolar (x, y) |> Tuple.first