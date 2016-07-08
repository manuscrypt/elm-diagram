module Dynamic.Diagram exposing (..)

import Dynamic.Layout as DynamicLayout
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import VirtualDom exposing (Node)
import Dynamic.SvgVisualization


{-
   --  SAMPLE
-}


type alias Node =
    { name : String
    }


type alias Connection =
    ( Node, Node )



-- MODEL


type alias Model =
    { nodes : List Node
    , rootNodes : List Node
    , connections : List Connection
    , layout : DynamicLayout.Model Node
    }


empty : Model
empty =
    { nodes = []
    , rootNodes = []
    , connections = []
    , layout =
        DynamicLayout.addForEachRule (DynamicLayout.noIntersection 100)
            <| DynamicLayout.addForOneRule (DynamicLayout.snapToGrid 100)
            <| DynamicLayout.empty
    }


addRootNode : Node -> Model -> Model
addRootNode rootNode model =
    { model
        | rootNodes = model.rootNodes ++ [ rootNode ]
        , nodes = model.nodes ++ [ rootNode ]
        , layout =
            DynamicLayout.addNodeAtPosition rootNode (vec2 (toFloat (List.length model.nodes) * 9) 100)
                <| List.foldr (\otherRoot -> DynamicLayout.addForTwoRule otherRoot (DynamicLayout.hasSameY 0.05) rootNode) model.layout model.rootNodes
    }


addNode : Node -> Model -> Model
addNode node model =
    { model
        | nodes =
            model.nodes ++ [ node ]
            --, layout = DynamicLayout.addNode node model.layout
        , layout = DynamicLayout.addNodeAtPosition node (vec2 (toFloat (List.length model.nodes) * 10) 0) model.layout
    }


containsNode : Node -> Model -> Bool
containsNode node model =
    if (0 == (List.length (List.filter (\n -> n == node) model.nodes))) then
        False
    else
        True


addImport : Node -> Node -> Model -> Model
addImport parent child model =
    { model
        | connections = model.connections ++ [ ( parent, child ) ]
        , layout =
            DynamicLayout.addForTwoRule child (DynamicLayout.isAbove 100) parent
                <| DynamicLayout.addForTwoRule child
                    (DynamicLayout.hasSameX 0.0005)
                    parent
                    model.layout
    }


animate : Model -> Model
animate model =
    { model
        | layout = DynamicLayout.animate model.layout
    }


svgNodes : Model -> List (VirtualDom.Node a)
svgNodes model =
    (Dynamic.SvgVisualization.grid (DynamicLayout.viewbox 50 model.layout))
        ++ (List.concat
                <| List.map
                    (\( nodeA, nodeB ) ->
                        Dynamic.SvgVisualization.connection (DynamicLayout.positionOfNode nodeB model.layout)
                            (DynamicLayout.positionOfNode nodeA model.layout)
                    )
                    model.connections
           )
        ++ (List.concat
                <| List.map
                    (\node ->
                        Dynamic.SvgVisualization.node (DynamicLayout.positionOfNode node model.layout)
                            node.name
                    )
                    model.nodes
           )
