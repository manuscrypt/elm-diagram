module Extra.Array exposing (getAt, setAt)

import Array exposing (get, set, Array)


getAt : Int -> Array number -> number
getAt idx arr =
    Array.get idx arr |> Maybe.withDefault 0


setAt : Int -> number -> Array number -> Array number
setAt idx val arr =
    Array.set idx val arr
