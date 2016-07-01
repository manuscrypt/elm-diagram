module GraphToDiagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Diagram
import Symbol
import Dict
import IntDict
import Connection
import Layout exposing (LayoutNode, LayoutCell)
import Graph exposing (Graph, NodeId, Node, Edge)
import Color
import Window


convert : ( Graph a b, Node a -> String ) -> Window.Size -> ( Diagram.Model, Layout.Model a b )
convert ( graph, labelFunc ) size =
    let
        layout =
            Layout.init graph labelFunc size

        nodes =
            List.map (createNode size) <| Dict.toList layout.cells

        ( syms, symsFx ) =
            createSymbols nodes

        conns =
            List.map (createConnection syms) (Graph.edges graph)

        ( dg, dgFx ) =
            Diagram.init syms conns
    in
        ( dg, layout )


createNode : Window.Size -> ( Int, LayoutCell a b ) -> LayoutNode
createNode size ( index, cell ) =
    let
        inc =
            IntDict.size cell.content.incoming

        out =
            IntDict.size cell.content.outgoing

        diff =
            inc - out

        spacing =
            80

        sgn =
            if ((%) index 2) == 0 then
                -1
            else
                1

        x =
            450 + sgn * (spacing + (toFloat ((%) index 3) * spacing / 2))

        y =
            spacing + (toFloat index * spacing)
    in
        LayoutNode cell.content.node.id (vec2 x y) (Color.white) (cell.labelFunc cell.content.node)


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
        <| List.filter (\s -> List.member s.id <| [ from, to ]) symbols
