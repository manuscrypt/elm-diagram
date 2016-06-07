module Util exposing (..)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )

vecToSvgPos : Vec2 -> String
vecToSvgPos vec = 
    (toString <| getX vec) ++ " " ++ (toString <| getY vec) 



warningsFor : String -> String
warningsFor moduleFileName =
    "elm-make --warn " ++ moduleFileName
