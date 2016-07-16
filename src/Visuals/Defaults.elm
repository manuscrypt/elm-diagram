module Visuals.Defaults exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Color exposing (Color)
import Extra.Svg exposing (Stroke)


defaultNodeSize : Vec2
defaultNodeSize =
    (vec2 20 20)


defaultNodeColor : Color
defaultNodeColor =
    Color.white


defaultConnectionStroke : Stroke
defaultConnectionStroke =
    Stroke Color.brown 1


defaultNodeStroke : Stroke
defaultNodeStroke =
    Stroke Color.black 1


defaultGridStroke : Stroke
defaultGridStroke =
    Stroke Color.lightBlue 0.15
