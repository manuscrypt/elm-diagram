module Layout exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Color exposing (Color)
import Dict exposing (Dict)
import IntDict
import Graph exposing (Graph, Node, NodeContext)
--import Extra.Graph exposing (noIncoming, incCount, outCount)


type alias LayoutNode =
    { id : Int
    , pos : Vec2
    , color : Color
    , label : String
    }


type alias LayoutCell a =
    { content : Node a
    , labelFunc : Node a -> String
    }


type alias Model a b =
    { graph : Graph a b
    , cells : Dict Int (LayoutCell a)
    , labelFunc : Node a -> String
    }


init : Graph a b -> (Node a -> String) -> Model a b
init g labelFunc =
    List.foldl addContent ({ graph = g, cells = Dict.empty, labelFunc = labelFunc })
        <| Graph.nodes g
        --<| List.map .node
--        <| noIncoming g


addContent : Node a -> Model a b -> Model a b
addContent content model =
    let
        withContent =
            case Dict.get content.id model.cells of
                Nothing ->
                    { model | cells = Dict.insert content.id (LayoutCell content model.labelFunc) model.cells }

                Just c ->
                    model

        deps =
            case Graph.get content.id withContent.graph of
                Nothing ->
                    []

                Just ctx ->
                    List.map .node
                        <| List.filterMap (\id -> Graph.get id withContent.graph) 
                        <| IntDict.keys ctx.outgoing
    in
        List.foldl addContent withContent deps


getCells : Model a b -> List (LayoutCell a)
getCells model =
    Dict.values <| model.cells


getCell : Model a b -> Node a -> Maybe (LayoutCell a)
getCell layout content =
    Dict.get content.id layout.cells
