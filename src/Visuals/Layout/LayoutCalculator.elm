module Visuals.Layout.LayoutCalculator exposing (..)

import Time exposing (Time)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub)
import Graph exposing (Node)

type alias Span = { min: Vec2, max: Vec2 }

type alias NodeView data out = Node data -> ( Span, out )

type alias LayoutCalculator state data out =
    { state : state
    , step : NodeView data out -> Time -> state -> state
    , at :  NodeView data out -> state -> Int -> ( Vec2, out )
    }
