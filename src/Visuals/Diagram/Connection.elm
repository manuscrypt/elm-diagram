module Visuals.Diagram.Connection exposing (..)

import Svg exposing (Svg)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add)
import Color exposing (Color)
import Extra.Svg exposing (Stroke, bezierLineWithDirection, arrow)
import Extra.Spline
import VirtualDom
import Model.BaseTypes exposing (Position)
import Visuals.Defaults exposing (defaultConnectionStroke)
import Visuals.Diagram.Node as Node


type alias Model n =
    { nodes : List (Node.Model n)
    , points : List Position
    , stroke : Stroke
    }


init : List (Node.Model n) -> Model n
init list =
    { nodes = list
    , points = []
    , stroke = defaultConnectionStroke
    }


view : Model n -> Svg a
view model =
    Svg.g [] <| createSplines model.nodes



--createEdge model.from.pos model.to.pos
-- or -> createSplines


createSplines : List (Node.Model n) -> List (VirtualDom.Node a)
createSplines nodes =
    List.map .pos nodes
        |> Extra.Spline.splines
        |> List.map (Extra.Svg.toPath "stroke:black;stroke-width:1px;fill:none")


createEdge : Position -> Position -> List (VirtualDom.Node a)
createEdge fromPos toPos =
    [ (bezierLineWithDirection (add fromPos (vec2 0 20))
        (vec2 0 40)
        (vec2 0 -60)
        (add toPos (vec2 0 -20))
        defaultConnectionStroke
      )
    , (arrow (add toPos (vec2 0 -20))
        (vec2 0 -1)
        defaultConnectionStroke
        Color.red
      )
    ]
