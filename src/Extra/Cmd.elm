module Extra.Cmd exposing (updateMany, updateOne, noFx)


noFx : a -> ( a, Cmd b )
noFx m =
    ( m, Cmd.none )


updateMany : List a -> (a -> b -> ( b, Cmd a )) -> ( b, Cmd a ) -> ( b, Cmd a )
updateMany msgs update modelCmd =
    List.foldl (updateOne update) modelCmd msgs


updateOne : (a -> b -> ( b, Cmd a )) -> a -> ( b, Cmd a ) -> ( b, Cmd a )
updateOne update msg ( model, effect ) =
    let
        ( next, nextEffect ) =
            update msg model
    in
        next ! [ effect, nextEffect ]
