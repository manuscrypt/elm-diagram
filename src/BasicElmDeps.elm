module BasicElmDeps exposing (..)

import CompilationUnit exposing (fromList)
import Graph exposing (Graph, Node)
import ElmFile exposing (ElmFile)


init : Graph ElmFile () -> ( Graph CompilationUnit.Model (), Node CompilationUnit.Model -> String )
init elmFiles =
    let
        basic =
            Graph.nodes elmFiles
                |> Debug.log "elms"
                |> List.filter (\n -> n.label.name == "Basic.elm")
                |> List.head
    in
        case basic of
            Nothing ->
                Debug.crash "No Basic.elm found"

            Just b ->
                let
                    smaller =
                        Graph.inducedSubgraph [ b.id ] elmFiles
                in
                    ( Graph.mapNodes CompilationUnit.init smaller
                    , (\a -> a.label.filename.name)
                    )



----------------------------------------
--  21-->22
--    +->23
--       23-->24
--    +->24


sample2 : ( Graph String (), Node String -> String )
sample2 =
    ( Graph.fromNodeLabelsAndEdgePairs [ "21", "22", "23", "24" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 2, 3 ), ( 0, 3 ) ]
    , (\x -> x.label)
    )
