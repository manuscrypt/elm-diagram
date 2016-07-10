module Old.Basic exposing (..)

import Html.App as App
import Graph exposing (Graph)
import Old.Visualization exposing (init, view, update)
import Old.ExampleGraphs as ExampleGraphs


main : Program Never
main =
    let
        ( g, fn ) =
            ExampleGraphs.simple
    in
        App.program
            { init = init g { width = 500, height = 500 } fn
            , view = view
            , update = update
            , subscriptions = (\_ -> Sub.none)
            }
