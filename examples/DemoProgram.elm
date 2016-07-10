module DemoProgram exposing (..)

import Html.App as App
import Demo.Model exposing (init, update, subscriptions)
import Demo.View exposing (view)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
