module Dependencies.Basic exposing (..)

import Html.App as App
import Visualization exposing (init, view, update)
import Graph exposing (Graph)


sample : ( Graph String (), { b | label : a } -> a )
sample =
    ( Graph.fromNodeLabelsAndEdgePairs [ "21", "22", "23", "24" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 2, 3 ), ( 0, 3 ) ]
    , (\x -> x.label)
    )


main : Program Never
main =
    let
        ( g, fn ) =
            sample
    in
        App.program
            { init = init g { width = 500, height = 500 } fn
            , view = view
            , update = update
            , subscriptions = (\_ -> Sub.none)
            }
