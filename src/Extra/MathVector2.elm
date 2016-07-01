module Extra.MathVector2 exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction, fromTuple)


rotate : Vec2 -> Float -> Vec2
rotate v g =
    let
        ix =
            getX v

        iy =
            getY v

        r =
            g * 0.017453293

        s =
            sin r

        c =
            cos r

        sx =
            s * ix

        cx =
            c * ix

        sy =
            s * iy

        cy =
            c * iy

        tx =
            cx - sy

        ty =
            sx + cy
    in
        vec2 tx ty


fromIntRecord : { a | x : Int, y : Int } -> Vec2
fromIntRecord { x, y } =
    fromTuple ( toFloat x, toFloat y )


fromIntTuple : ( Int, Int ) -> Vec2
fromIntTuple ( x, y ) =
    fromIntRecord { x = x, y = y }


toIntRecord : Vec2 -> { x : Int, y : Int }
toIntRecord v =
    { x = round <| getX v, y = round <| getY v }


multiplyVec : Vec2 -> Vec2 -> Vec2
multiplyVec v1 v2 =
    vec2 (getX v1 * getX v2) (getY v1 * getY v2)
