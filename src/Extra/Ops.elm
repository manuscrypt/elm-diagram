module Extra.Ops exposing ((=>), attrs)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list
