module Layouts.Manuel exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Graph exposing (Graph, Node, NodeId, NodeContext)
import Model.LayoutConfig as Config
import Model.Layout as Layout exposing (PosAndLabel)


-- MODEL


type alias Model =
    Config.Model PosAndLabel LayoutData


type alias LayoutData =
    { graph : Graph PosAndLabel ()
    }


init : Graph ElmFile e -> (ElmFile -> String) -> Model
init graph labelFn =
    let
        mapped =
            Graph.mapNodes (\n -> (,) (vec2 (50 + (toFloat n.id) * 10) 250) (labelFn n)) graph

        mapped' =
            Graph.mapEdges (\e -> ()) mapped
    in
        Config.init { graph = mapped' }
            (\n m -> m)
            (\n m -> False)
            (\n n m -> m)
            animate


position : NodeId -> Model -> Vec2
position nodeId model =
    case Graph.get nodeId model.graph of
        Just ctx ->
            fst ctx.node.label

        Nothing ->
            Debug.crash <| "node not found with id: " ++ toString nodeId


animate : LayoutData -> LayoutData
animate layout =
    layout



-- hier kommt die magic
