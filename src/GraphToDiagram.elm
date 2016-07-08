module GraphToDiagram exposing (..)

import Window
import Color
import Dict
import IntDict
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Graph exposing (Graph, NodeId, Node, Edge)
import Visuals.Diagram as Diagram
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Layout.Layout as Layout exposing (LayoutNode, LayoutCell)


convert : ( Graph a b, Node a -> String ) -> Window.Size -> ( Diagram.Model, Layout.Model a b )
convert ( graph, labelFunc ) size =
    let
        layout =
            Layout.init graph labelFunc size

        nodes =
            List.map (createNode size <| Graph.size graph) <| Dict.toList layout.cells

        ( syms, symsFx ) =
            createSymbols nodes

        conns =
            List.map (createConnection syms) (Graph.edges graph)

        ( dg, dgFx ) =
            Diagram.init (vec2 (toFloat size.width) (toFloat size.height)) syms conns
    in
        ( dg, layout )


createNode : Window.Size -> Int -> ( Int, LayoutCell a b ) -> LayoutNode
createNode size count ( index, cell ) =
    let
        inc =
            IntDict.size cell.content.incoming

        out =
            IntDict.size cell.content.outgoing

        slice =
            degrees (360 / (toFloat count))

        ( hw, hh ) =
            ( toFloat size.width, toFloat size.height )

        mx =
            Basics.max hw hh

        ( fw, fh ) =
            --( mx, mx )
            ( hw, hh )

        x =
            (fw / 2) + (fw / 4) * sin (toFloat index * slice)

        y =
            (fh / 2) - (fh / 4) * cos (toFloat index * slice)
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
