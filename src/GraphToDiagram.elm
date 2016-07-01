
module GraphToDiagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Diagram
import Symbol
import Dict
import Connection
import Layout exposing (LayoutNode, LayoutCell)
import Graph exposing (Graph, NodeId, Node, Edge)
import Extra.Graph exposing (incCount, outCount)
import KellyColors

convert : ( Graph a b, Node a -> String ) -> (Diagram.Model, Layout.Model a b)
convert ( graph, labelFunc ) =
    let
        layout =
            Layout.init graph labelFunc

        nodes =
            List.indexedMap (createNode layout) <| Dict.values layout.cells

        ( syms, symsFx ) =
            createSymbols nodes

        conns = List.map (createConnection syms) (Graph.edges graph) 

        ( dg, dgFx ) =
            Diagram.init syms conns
    in
        (dg, layout)

createNode : Layout.Model a b -> Int -> LayoutCell a -> LayoutNode
createNode model index cell =
    let
        inc = (incCount cell.content model.graph)
        out = (outCount cell.content model.graph)
        diff = inc - out
        x =
            if( (%) index 2 == 0 ) then 
                450 + ((toFloat index) * 60)
            else 
                450 - ((toFloat index) * 60)

        y = 
            450 + (toFloat (out * 30) + (toFloat (inc * -30)))
    in
        LayoutNode cell.content.id (vec2 x y) (KellyColors.at index) (cell.labelFunc cell.content)


nodeToSymbol : Layout.LayoutNode -> ( Symbol.Model, Cmd Symbol.Msg )
nodeToSymbol node =
    Symbol.init node.id node.label node.color (vec2 20 20) node.pos

createSymbols : List Layout.LayoutNode -> ( List Symbol.Model, List (Cmd Symbol.Msg) )
createSymbols nodes =
    List.unzip <| List.map nodeToSymbol nodes

createConnections : List Symbol.Model -> List (Edge b) -> List Connection.Model
createConnections syms edges =
    List.map (createConnection syms) edges

createConnection : List Symbol.Model -> Edge b -> Connection.Model
createConnection symbols { from, to } =
    Connection.init 
        <| List.filter (\s -> List.member s.id <| [from, to]) symbols