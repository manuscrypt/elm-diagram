module Extra.Style exposing (..)

import String exposing (join)
import List exposing (map)
import Extra.Ops exposing ((=>))


defaultStroke : List ( String, String )
defaultStroke =
    [ "stroke" => "black", "stroke-width" => "1px" ]


cursorHand : List ( String, String )
cursorHand =
    [ "cursor" => "hand" ]


unselectable : List ( String, String )
unselectable =
    [ "-webkit-touch-callout" => "none"
    , "-webkit-user-select" => "none"
    , "-khtml-user-select" => "none"
    , "-moz-user-select" => "none"
    , "-ms-user-select" => "none"
    , "user-select" => "none"
    ]


fromList : List ( String, String ) -> String
fromList list =
    join "; " <| map (\( k, v ) -> k ++ ": " ++ v) list
