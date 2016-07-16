module Model.BaseTypes exposing (DeltaTime, Size, Position, Velocity, Acceleration, Angle, Mass, Force)

import Math.Vector2 as Vec2 exposing (Vec2)
import Time exposing (Time)


type alias DeltaTime =
    Time


type alias Position =
    Vec2


type alias Velocity =
    Vec2


type alias Force =
    Vec2


type alias Acceleration =
    Vec2


type alias Size =
    Vec2


type alias Angle =
    Float


type alias Mass =
    Float
