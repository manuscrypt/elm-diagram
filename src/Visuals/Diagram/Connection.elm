module Visuals.Diagram.Connection exposing (..)

import Svg exposing (Svg)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add)
import Color exposing (Color)
import Extra.Svg exposing (Stroke, bezierLineWithDirection, arrow)
import Extra.Spline
import VirtualDom
import Model.BaseTypes exposing (Position)
import Visuals.Defaults exposing (defaultConnectionStroke)
import Visuals.Layout.Force.Body as Body


type alias Model =
    { nodes : List (Body.Model)
    , points : List Position
    , stroke : Stroke
    }


init : List (Body.Model) -> Model
init list =
    { nodes = list
    , points = []
    , stroke = defaultConnectionStroke
    }


view : Model -> Svg a
view model =
  case List.map .pos model.nodes of
    [a,b] ->
      Svg.g [] (createEdge a b)
    _ ->
      Svg.g [] (createSplines model.nodes)

createSplines : List (Body.Model) -> List (Svg a)
createSplines nodes =
    List.map .pos nodes
        |> Extra.Spline.splines
        |> List.map (Extra.Svg.toPath "stroke:black;stroke-width:1px;fill:none")


createEdge : Position -> Position -> List (Svg a)
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
