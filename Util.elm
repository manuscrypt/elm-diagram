module Util exposing (..)


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )


warningsFor : String -> String
warningsFor moduleFileName =
    "elm-make --warn " ++ moduleFileName
