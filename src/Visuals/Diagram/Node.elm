module Visuals.Diagram.Node exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale)
import Model.BaseTypes exposing (DeltaTime, Position, Velocity, Size, Force, Acceleration, Angle, Mass)
import Color exposing (Color)
import Extra.Svg exposing (Stroke, translate)
import Visuals.Defaults exposing (defaultNodeSize, defaultNodeColor, defaultNodeStroke)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Extra.Svg exposing (translate)


type alias Model n =
    { id : Int
    , data : n
    , labelFunc : n -> String
    , pos : Position
    , size : Size
    , color : Color
    , stroke : Stroke
    , vel : Velocity
    , force : Force
    , accel : Acceleration
    , angle : Angle
    , mass : Mass
    }


zero : Vec2
zero =
    vec2 0 0


init : Int -> n -> (n -> String) -> Position -> Model n
init id data labelFunc pos =
    { id = id
    , data = data
    , labelFunc = labelFunc
    , pos = pos
    , size = defaultNodeSize
    , color = defaultNodeColor
    , stroke = defaultNodeStroke
    , vel = zero
    , force = zero
    , accel = zero
    , angle = 0
    , mass =
        1
        --kg
    }


forward : DeltaTime -> Model n -> Model n
forward dt body =
    let
        accel =
            add body.accel (scale (1 / body.mass) body.force)
    in
        { body
            | pos = pos' body.pos body.vel accel dt
            , vel = vel' body.vel accel dt
            , accel = accel
            , force = zero
        }


view : Model n -> Svg a
view model =
    Svg.g [ SA.transform <| translate model.pos ]
        [ Extra.Svg.circle (vec2 0 0) (getX model.size) model.stroke model.color
        , Extra.Svg.textCentered (vec2 0 0) "font-weight:bold; font-size:20px; font-family: Verdana; fill: black" <| model.labelFunc model.data
        ]



----- KINEMATICS

friction = 12.9501

vel' : Velocity -> Acceleration -> DeltaTime -> Velocity
vel' v0 a t =
    add (scale ( max 0.0 ( 1.0 - ( t * friction ) ) ) v0 ) 
        (scale t a)


pos' : Position -> Velocity -> Acceleration -> DeltaTime -> Position
pos' p0 v0 a t =
    add (add p0 (scale t v0)) (scale (0.5 * t * t) a)
