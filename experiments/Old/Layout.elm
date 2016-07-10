module Old.Layout exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Color exposing (Color)
import Dict exposing (Dict)
import Graph exposing (Graph, Node, NodeContext)
import Window


type alias LayoutNode =
    { id : Int
    , pos : Vec2
    , color : Color
    , label : String
    }


type alias LayoutCell a b =
    { content : NodeContext a b
    , labelFunc : Node a -> String
    }


type alias Model a b =
    { graph : Graph a b
    , cells : Dict Int (LayoutCell a b)
    , labelFunc : Node a -> String
    , size : Window.Size
    }


init : Graph a b -> (Node a -> String) -> Window.Size -> Model a b
init g labelFunc size =
    let
        sorted =
            Graph.topologicalSort g

        cells =
            Dict.fromList <| List.indexedMap (\i ctx -> ( i, LayoutCell ctx labelFunc )) <| sorted
    in
        { graph = g
        , cells = cells
        , labelFunc = labelFunc
        , size = size
        }


getCells : Model a b -> List (LayoutCell a b)
getCells model =
    Dict.values <| model.cells


getCell : Model a b -> Node a -> Maybe (LayoutCell a b)
getCell layout content =
    Dict.get content.id layout.cells
