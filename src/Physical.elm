module Physical exposing (..)

import Constants
import Vector exposing (Vector)

type alias HasPosition a =
    { a
    | position: Vector
    , angle: Vector -- Unit vector
    }

type alias HasVelocity a =
    { a
    | velocity: Vector
    , angularSpeed: Float -- rad/sec
    }

type alias HasMass a =
    { a
    | mass: Float
    }

type alias HasSize a =
    { a
    | radius: Float
    }

type alias Physical a =
    HasSize (HasPosition (HasVelocity (HasMass a)))

setPosition : Vector -> HasPosition a -> HasPosition a
setPosition pos obj =
    { obj | position = pos }


setAngle : Vector -> HasPosition a -> HasPosition a
setAngle angle obj =
    { obj | angle = angle }


setVelocity : Vector -> HasVelocity a -> HasVelocity a
setVelocity velocity obj =
    { obj | velocity = velocity }


setAngularSpeed : Float -> HasVelocity a -> HasVelocity a
setAngularSpeed angularSpeed obj =
    { obj | angularSpeed = angularSpeed }


updatePosition : Float -> HasPosition (HasVelocity a) -> HasPosition (HasVelocity a)
updatePosition deltaT obj =
    let
        deltaP = Vector.scale deltaT obj.velocity
        position_ = Vector.add obj.position deltaP
        deltaR = deltaT * obj.angularSpeed
        angle_ = Vector.rotate deltaR obj.angle 
    in
        { obj | position = position_, angle = angle_ }


accelerate : Vector -> HasVelocity (HasMass a) -> HasVelocity (HasMass a)
accelerate totalForce obj =
    let
        deltaV = Vector.scale (1/obj.mass) totalForce
        velocity_ = Vector.add obj.velocity deltaV
    in
        { obj | velocity = velocity_ }


applyForce : Float -> Vector -> HasVelocity (HasMass a) -> HasVelocity (HasMass a)
applyForce deltaT force obj =
    accelerate (Vector.scale deltaT force) obj


accelerateAngle : Float -> HasVelocity (HasMass a) -> HasVelocity (HasMass a)
accelerateAngle totalAngularForce obj =
    let
        deltaV = totalAngularForce / obj.mass
        angularSpeed_ = obj.angularSpeed + deltaV
    in
        { obj | angularSpeed = angularSpeed_ }


applyAngularForce : Float -> Float -> HasVelocity (HasMass a) -> HasVelocity (HasMass a)
applyAngularForce deltaT force obj =
    accelerateAngle (deltaT * force) obj


clampPosition : HasPosition a -> HasPosition a
clampPosition obj =
    let
        (x,y) = Vector.toXY obj.position
        x_ = modFloatBy Constants.worldWidth x
        y_ = modFloatBy Constants.worldWidth y
        position_ = Vector.Cartesian { x = x_, y = y_ }
    in
        { obj | position = position_ }


modFloatBy : Int -> Float -> Float
modFloatBy base n =
    if n < 0
        then
            modFloatBy base (n + toFloat base)
        else
            if n > toFloat base
                then modFloatBy base (n - toFloat base)
                else n


vectorBetween : HasPosition a -> HasPosition b -> Vector
vectorBetween obj1 obj2 =
    Vector.subtract obj2.position obj1.position -- Should be vector from obj1 to obj2


overlap : HasPosition (HasSize a) -> HasPosition (HasSize b) -> Maybe Vector
overlap obj1 obj2 =
    let
        difference = vectorBetween obj1 obj2
        distance = Vector.magnitude difference
        isCollision = distance < obj1.radius + obj2.radius
    in
        if isCollision
            then Just difference
            else Nothing


reflect : Vector -> HasVelocity a -> HasVelocity a
reflect normalVec obj =
    let
        normal = Vector.toUnit normalVec
        normalScaling = 2 * (Vector.dot obj.velocity normal)
        velocity_ = 
            Vector.subtract
                obj.velocity
                (Vector.scale normalScaling normal)
    in
        { obj | velocity = velocity_ }

collide : Physical a -> Physical b -> (Physical a, Physical b)
collide obj1 obj2 =
    let
        positionDiff = Vector.subtract obj1.position obj2.position
        velocityDiff = Vector.subtract obj1.velocity obj2.velocity
        massCoeff    = 2 / (obj1.mass + obj2.mass)

        distance = Vector.magnitude positionDiff

        coeff = massCoeff * (Vector.dot velocityDiff positionDiff) / distance ^ 2

        v1_ =
            Vector.subtract
                obj1.velocity
                (Vector.scale
                    (coeff * obj1.mass)
                    positionDiff)

        v2_ =
            Vector.subtract
                obj2.velocity
                (Vector.scale
                    (coeff * obj2.mass)
                    (Vector.negate positionDiff))
    in
        ({ obj1 | velocity = v1_ }, { obj2 | velocity = v2_ })
    