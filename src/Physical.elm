module Physical exposing (..)

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
    | size: Float
    }

type alias Physical a =
    HasPosition (HasVelocity (HasMass a))

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