module Layout exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Color exposing (Color)
import Dict exposing (Dict)
import Graph exposing (Graph,Node,NodeContext)
import Focus exposing ((=>))

type alias LayoutNode =
    { pos : Vec2
    , color : Color
    }

type alias LayoutCell a =
    { content : Node a
    , index : Int
    }


type alias Model a =
    { graph : Graph a
    , cells : Dict Int (LayoutCell a)
    }


init : Graph a b -> Model a
init g =
    List.foldl addContent ({ graph = g, cells = Dict.empty }) <| DependencyGraph.nodesWithoutIncoming g


addContent : Node a -> Model a -> Model a
addContent content model =
    let
        newIndex =
            Dict.size model.cells

        newCell =
            LayoutCell content newIndex

        withContent =
            { model | cells = Dict.insert newIndex newCell model.cells }

        ctx = Node

        focus = Graph.outgoing => Focus.withDefault model.graph => Graph.node
        graph' = Focus.set focus model.graph

        deps = Focus.get focus graph'
            DependencyGraph.outgoingNodes content model.graph
    in
        List.foldl addContent withContent deps


getNodes : Model a -> List LayoutNode
getNodes model =
    model.cells
        |> Dict.values
        |> List.indexedMap createNode


createNode : Int -> LayoutCell a -> LayoutNode
createNode index cell =
    let
        x =
            30 + ((toFloat index) * 80)

        y =
            30 + ((toFloat index) * 80)
    in
        LayoutNode (vec2 x y) Color.red



getCells : Model a -> List (LayoutCell a)
getCells model =
    model.cells
        |> Dict.values


getCell : Model a -> Vertex a -> Maybe (LayoutCell a)
getCell layout content =
    layout.cells
        |> Dict.values
        |> List.filter (((==) content) << .content)
        |> List.head
