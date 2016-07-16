module DemoProgram exposing (..)

import Html.App as App
import Demo.Model exposing (init, update, subscriptions)
import Demo.View exposing (view)
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Model.ElmFile exposing (ElmFile)


sample1 : ElmFileGraph
sample1 =
    ElmFileGraph.fromFiles
        [ ElmFile 21 "21" "/path/21" 21 [ "22", "23", "24" ] "21"
        , ElmFile 22 "22" "/path/22" 22 [ "24" ] "22"
        , ElmFile 23 "23" "/path/23" 23 [ "24" ] "23"
        , ElmFile 24 "24" "/path/24" 24 [] "24"
        ]


main : Program Never
main =
    App.program
        { init = init sample1
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
