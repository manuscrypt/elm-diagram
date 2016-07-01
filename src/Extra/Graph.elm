module Extra.Graph exposing (isAcyclic, noIncoming)

import Graph exposing (Graph, NodeContext)
import IntDict

isAcyclic: Graph a b -> Bool
isAcyclic graph = 
    let hasLoop ctx = IntDict.member ctx.node.id ctx.incoming
    in Graph.fold (\ctx acc -> acc || hasLoop ctx) False graph == False

noIncoming: Graph a b -> List (NodeContext a b) 
noIncoming g = 
    let noInc ctx = (IntDict.size ctx.incoming == 0)
    in Graph.fold (\ctx acc -> if noInc ctx then acc ++ [ctx] else acc) [] g