module Dynamic.Diagram exposing (..)

import Layouts.Rules as RulesLayout
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Visuals.Grid as Grid
import Dynamic.SvgVisualization as SvgVisualization
import VirtualDom


{-
   --  SAMPLE
-}


type alias Node =
    { name : String
    }


type alias Connection =
    ( Node, Node )



-- MODEL


type Msg
    = NoOp


type alias Model =
    { nodes : List Node
    , rootNodes : List Node
    , connections : List Connection
    , layout : RulesLayout.Model Node
    , grid : Grid.Model
    }


init : Model
init =
    { nodes = []
    , rootNodes = []
    , connections = []
    , layout =
        RulesLayout.addForEachRule (RulesLayout.noIntersection 100)
            <| RulesLayout.addForOneRule (RulesLayout.snapToGrid 100)
            <| RulesLayout.empty
    , grid = Grid.empty
    }


update : Msg -> Model -> Model
update msg model =
    model


addRootNode : Node -> Model -> Model
addRootNode rootNode model =
    { model
        | rootNodes = model.rootNodes ++ [ rootNode ]
        , nodes = model.nodes ++ [ rootNode ]
        , layout =
            RulesLayout.addNodeAtPosition rootNode (vec2 (toFloat (List.length model.nodes) * 9) 100)
                <| List.foldr (\otherRoot -> RulesLayout.addForTwoRule otherRoot (RulesLayout.hasSameY 0.05) rootNode) model.layout model.rootNodes
    }


addNode : Node -> Model -> Model
addNode node model =
    { model
        | nodes =
            model.nodes ++ [ node ]
            --, layout = RulesLayout.addNode node model.layout
        , layout = RulesLayout.addNodeAtPosition node (vec2 (toFloat (List.length model.nodes) * 10) 0) model.layout
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
            RulesLayout.addForTwoRule child (RulesLayout.isAbove 100) parent
                <| RulesLayout.addForTwoRule child
                    (RulesLayout.hasSameX 0.0005)
                    parent
                    model.layout
    }


animate : Model -> Model
animate model =
    { model
        | layout = RulesLayout.animate model.layout
    }


view : Model -> List (VirtualDom.Node a)
view model =
    let
        ( minx, miny, maxx, maxy ) =
            RulesLayout.viewbox 50 model.layout

        grid =
            Grid.init ( minx, maxx ) ( miny, maxy ) 25
    in
        [ Grid.view grid ]
            ++ (List.concat
                    <| List.map
                        (\( nodeA, nodeB ) ->
                            SvgVisualization.connection (RulesLayout.positionOfNode nodeB model.layout)
                                (RulesLayout.positionOfNode nodeA model.layout)
                        )
                        model.connections
               )
            ++ (List.concat
                    <| List.map
                        (\node ->
                            SvgVisualization.node (RulesLayout.positionOfNode node model.layout)
                                node.name
                        )
                        model.nodes
               )
