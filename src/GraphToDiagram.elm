module GraphToDiagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Diagram
import Symbol
import Connection
import Layout exposing (LayoutNode, LayoutCell)
import Graph exposing (Graph, Node, Edge)

convert : ( Graph a b, Node a -> String ) -> (Diagram.Model, Layout.Model a b)
convert ( graph, labelFunc ) =
    let
        layout =
            Layout.init graph labelFunc

        nodes =
            Layout.getNodes layout

        ( syms, symsFx ) =
            createSymbols nodes

        conns =
            createConnections syms layout graph

        ( dg, dgFx ) =
            Diagram.init syms conns
    in
        (dg, layout)


nodeToSymbol : Int -> Layout.LayoutNode -> ( Symbol.Model, Cmd Symbol.Msg )
nodeToSymbol index node =
    Symbol.init index node.label node.color (vec2 20 20) node.pos


createSymbols : List Layout.LayoutNode -> ( List Symbol.Model, List (Cmd Symbol.Msg) )
createSymbols nodes =
    List.unzip <| List.indexedMap nodeToSymbol nodes


createConnections : List Symbol.Model -> Layout.Model a b -> Graph a b -> List Connection.Model
createConnections syms layout graph =
    let
        edges =
            Graph.edges graph
    in
        List.foldl (createConnection syms layout graph) [] edges


createConnection : List Symbol.Model -> Layout.Model a b -> Graph a b -> Edge b -> List Connection.Model -> List Connection.Model
createConnection symbols layout graph edge conns =
    let
        cells =
            edgeToCells edge layout graph
    in
        conns ++ [ Connection.init <| cellsToSymbols cells symbols ]


edgeToCells : Edge b -> Layout.Model a b -> Graph a b -> List (Layout.LayoutCell a)
edgeToCells e layout graph =
    let
        vs =
            List.map .node <| List.filterMap (\id -> Graph.get id graph) [ e.from, e.to ]
    in
        List.filterMap (Layout.getCell layout) vs


cellsToSymbols : List (Layout.LayoutCell a) -> List Symbol.Model -> List Symbol.Model
cellsToSymbols cells symbols =
    let
        idxs =
            List.map .index cells
    in
        List.filter (\s -> List.member s.id idxs) symbols
