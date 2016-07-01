module ElmFileGraph exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import String 
import ElmFile exposing (ElmFile, decodeList) 

type alias ElmFileGraph = Graph ElmFile ()

fromFiles: List ElmFile -> ElmFileGraph
fromFiles files =
    let nodes = List.map (\o -> Node o.id o) files
        edges = makeEdges files 
    in Graph.fromNodesAndEdges nodes edges

makeEdges: List ElmFile -> List (Edge ())
makeEdges files =
    List.foldl (makeEdge files) [] files

makeEdge: List ElmFile -> ElmFile -> List (Edge ())-> List (Edge ())
makeEdge all file edges = 
    let imps = List.filterMap (findFile all) file.imports
    in edges ++ (List.map (\f -> Edge f.id file.id ()) imps) 

findFile: List ElmFile -> String -> Maybe ElmFile
findFile files name =
    Maybe.oneOf 
        [ List.head <| List.filter (\f -> f.moduleName == name ) files
        , List.head <| List.filter (\f -> (String.dropRight 4 f.name) == name ) files
        ]