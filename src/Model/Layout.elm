module Model.Layout exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Graph exposing (Graph, Node, NodeId, NodeContext)


-- MODEL


type alias Model =
    { graph : Graph Vec2 ()
    }


init : Graph n e -> Model
init graph =
    let mapped = Graph.mapNodes (\n ->  (vec2 100 100) ) graph
        mapped' = Graph.mapEdges (\e -> ()) mapped
    in { graph = mapped' }


position : NodeId -> Model -> Vec2
position nodeId model =
    case Graph.get nodeId model.graph of
        Just ctx ->
            ctx.node.label

        Nothing ->
            Debug.crash <| "node not found with id: " ++ toString nodeId


animate : Model -> Model
animate layout =
    layout



-- hier kommt die magic
