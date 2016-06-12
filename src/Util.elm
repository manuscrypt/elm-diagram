module Util exposing (..)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Array exposing (get,set,Array)


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )

vecToSvgPos : Vec2 -> String
vecToSvgPos vec = 
    (toString <| getX vec) ++ " " ++ (toString <| getY vec) 



warningsFor : String -> String
warningsFor moduleFileName =
    "elm-make --warn " ++ moduleFileName

getAt: Int->Array number->number    
getAt idx arr = Array.get idx arr |> Maybe.withDefault 0
setAt: Int->number->Array number->Array number    
setAt idx val arr = Array.set idx val arr 


updateMany : List a -> (a->b->(b, Cmd a))-> ( b, Cmd a ) -> ( b, Cmd a )
updateMany msgs update modelCmd =
    List.foldl (updateOne update) modelCmd msgs


updateOne : (a->b->(b, Cmd a))-> a -> ( b, Cmd a ) -> ( b, Cmd a )
updateOne update msg ( model, effect ) =
    let
        ( next, nextEffect ) =
            update msg model
    in
        next ! [ effect, nextEffect ]
