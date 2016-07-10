module Extra.Graph exposing (isAcyclic, noIncoming, incCount, outCount, getNodes)

import Graph exposing (Graph, NodeContext, Node, NodeId)
import IntDict


getNodes : List NodeId -> Graph a b -> List (NodeContext a b)
getNodes nodeIds graph =
    List.filterMap (\id -> Graph.get id graph) nodeIds

isAcyclic : Graph a b -> Bool
isAcyclic graph =
    let
        hasLoop ctx =
            IntDict.member ctx.node.id ctx.incoming
    in
        Graph.fold (\ctx acc -> acc || hasLoop ctx) False graph == False


noIncoming : Graph a b -> List (NodeContext a b)
noIncoming g =
    let
        noInc ctx =
            (IntDict.size ctx.incoming == 0)
    in
        Graph.fold
            (\ctx acc ->
                if noInc ctx then
                    acc ++ [ ctx ]
                else
                    acc
            )
            []
            g


incCount : Node a -> Graph a b -> Int
incCount node graph =
    Maybe.withDefault 0
        <| Maybe.map (\ctx -> IntDict.size ctx.incoming)
        <| Graph.get node.id graph


outCount : Node a -> Graph a b -> Int
outCount node graph =
    Maybe.withDefault 0
        <| Maybe.map (\ctx -> IntDict.size ctx.outgoing)
        <| Graph.get node.id graph
