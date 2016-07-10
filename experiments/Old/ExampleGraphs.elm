module Old.ExampleGraphs exposing (..)

import Graph exposing (Graph)


simple : ( Graph String (), { b | label : a } -> a )
simple =
    ( Graph.fromNodeLabelsAndEdgePairs [ "21", "22", "23", "24" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 2, 3 ), ( 0, 3 ) ]
    , (\x -> x.label)
    )
