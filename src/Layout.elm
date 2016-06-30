module Layout exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Color exposing (Color)
import Dict exposing (Dict)
import IntDict
import Graph exposing (Graph,Node,NodeContext)

type alias LayoutNode =
    { pos : Vec2
    , color : Color
    , label : String
    }

type alias LayoutCell a =
    { content : Node a
    , index : Int
    , labelFunc: (Node a->String)
    }


type alias Model a b =
    { graph : Graph a b
    , cells : Dict Int (LayoutCell a)
    , labelFunc: (Node a->String)
    }

init : Graph a b -> (Node a->String) -> Model a b
init g labelFunc =
    List.foldl addContent ({ graph = g, cells = Dict.empty, labelFunc = labelFunc }) 
        <| List.map .node <| noIncoming g

noIncoming: Graph a b -> List (NodeContext a b) 
noIncoming g = 
    let noInc ctx = (IntDict.size ctx.incoming == 0)
    in Graph.fold (\ctx acc -> if noInc ctx then acc ++ [ctx] else acc) [] g

addContent : Node a -> Model a b -> Model a b
addContent content model =
    let
        newIndex =
            Dict.size model.cells

        newCell =
            LayoutCell content newIndex model.labelFunc

        withContent =
            { model | cells = Dict.insert newIndex newCell model.cells }

        deps = 
            case Graph.get content.id model.graph of
                Nothing ->
                    []
                Just ctx ->
                    let outIds = ctx.outgoing 
                        outCtxs = IntDict.map (\id x-> Graph.get id model.graph) outIds
                    in List.map .node <| List.filterMap identity <| IntDict.values outCtxs
    in
        List.foldl addContent withContent deps


getNodes : Model a b -> List LayoutNode
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

        label = cell.labelFunc cell.content
    in
        LayoutNode (vec2 x y) Color.red label



getCells : Model a b -> List (LayoutCell a)
getCells model =
    model.cells
        |> Dict.values


getCell : Model a b -> Node a -> Maybe (LayoutCell a)
getCell layout content =
    layout.cells
        |> Dict.values
        |> List.filter (((==) content) << .content)
        |> List.head
