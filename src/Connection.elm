module Connection exposing (..)

import Svg exposing (Svg)
import VirtualDom exposing (Node)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Color exposing (Color)
import Extra.Svg exposing (Stroke, bezierLineWithDirection, arrow)
import Symbol


type alias Model =
    { symbols : List Symbol.Model
    , width : Int
    , stroke : Color
    }


blackStroke : Stroke
blackStroke =
    Stroke Color.brown 1


init : List Symbol.Model -> Model
init symbols =
    Model symbols 3 Color.black


view : Model -> Svg a
view model =
    Svg.g [] (createEdges model.symbols)


createEdges : List Symbol.Model -> List (VirtualDom.Node a)
createEdges nodes =
    case nodes of
        a :: b :: _ ->
            (createEdge a b) ++ (createEdges (List.drop 1 nodes))

        _ ->
            []


createEdge : Symbol.Model -> Symbol.Model -> List (VirtualDom.Node a)
createEdge fromModel toModel =
    [ (bezierLineWithDirection (Math.Vector2.add fromModel.pos (vec2 0 20))
        (vec2 0 40)
        (vec2 0 -60)
        (Math.Vector2.add toModel.pos (vec2 0 -20))
        blackStroke
      )
    , (arrow (Math.Vector2.add toModel.pos (vec2 0 -20))
        (vec2 0 -1)
        blackStroke
        Color.red
      )
    ]
