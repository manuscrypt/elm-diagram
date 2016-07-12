module Layouts.Reto exposing (..)

import Model.LayoutConfig as Config
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Layouts.Rules as RulesLayout


type alias Node =
    { name : String
    }


type alias Connection =
    ( Node, Node )


type alias LayoutData =
    { nodes : List Node
    , rootNodes : List Node
    , connections : List Connection
    , layout : RulesLayout.Model Node
    }


type alias Model =
    Config.Model Node LayoutData


init : Model
init =
    Config.init empty addRootNode addNode containsNode addImport animate


empty : LayoutData
empty =
    { nodes = []
    , rootNodes = []
    , connections = []
    , layout = defaultLayout
    }


defaultLayout : RulesLayout.Model Node
defaultLayout =
    RulesLayout.addForEachRule (RulesLayout.noIntersection 100)
        <| RulesLayout.addForOneRule (RulesLayout.snapToGrid 100)
        <| RulesLayout.init


addRootNode : Node -> LayoutData -> LayoutData
addRootNode rootNode model =
    { model
        | rootNodes = model.rootNodes ++ [ rootNode ]
        , nodes = model.nodes ++ [ rootNode ]
        , layout =
            RulesLayout.addNodeAtPosition rootNode (vec2 (toFloat (List.length model.nodes) * 9) 100)
                <| List.foldr (\otherRoot -> RulesLayout.addForTwoRule otherRoot (RulesLayout.hasSameY 0.05) rootNode) model.layout model.rootNodes
    }


addNode : Node -> LayoutData -> LayoutData
addNode node model =
    { model
        | nodes =
            model.nodes ++ [ node ]
            --, layout = RulesLayout.addNode node model.layout
        , layout = RulesLayout.addNodeAtPosition node (vec2 (toFloat (List.length model.nodes) * 10) 0) model.layout
    }


containsNode : Node -> LayoutData -> Bool
containsNode node model =
    if (0 == (List.length (List.filter (\n -> n == node) model.nodes))) then
        False
    else
        True


addImport : Node -> Node -> LayoutData -> LayoutData
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


animate : LayoutData -> LayoutData
animate model =
    { model
        | layout = RulesLayout.animate model.layout
    }
