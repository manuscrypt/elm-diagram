module Extra.Spline exposing (..)

import List
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, negate)
import Array exposing (Array, fromList)
import Extra.Svg exposing (..)
import Extra.Array exposing (..)


type alias VecN =
    Array Float


splines : List Vec2 -> List String
splines points =
    let
        xs =
            getXs points

        ys =
            getYs points

        px =
            computeControlPoints xs

        py =
            computeControlPoints ys
    in
        List.map (toPath xs ys px py) [0..(List.length points - 2)]


toPath : VecN -> VecN -> { p1 : VecN, p2 : VecN } -> { p1 : VecN, p2 : VecN } -> Int -> String
toPath xs ys px py idx =
    let
        from =
            vec2 (getAt idx xs) (getAt idx ys)

        p1' =
            vec2 (getAt idx px.p1) (getAt idx py.p1)

        p2' =
            vec2 (getAt idx px.p2) (getAt idx py.p2)

        to =
            vec2 (getAt (idx + 1) xs) (getAt (idx + 1) ys)
    in
        "M " ++ sp from ++ " C " ++ sp p1' ++ " " ++ sp p2' ++ " " ++ sp to


getXs : List Vec2 -> VecN
getXs points =
    Array.map getX <| Array.fromList points


getYs : List Vec2 -> VecN
getYs points =
    Array.map getY <| Array.fromList points


computeControlPoints : VecN -> { p1 : VecN, p2 : VecN }
computeControlPoints k =
    let
        n =
            (Array.length k) - 1

        gk i =
            getAt i k

        a =
            Array.initialize n (always 0)

        b =
            Array.initialize n (always 2)

        c =
            Array.initialize n (always 1)

        r =
            Array.initialize n (always (gk 0 + 2 * (gk 1)))

        a' =
            List.foldl (\i ax -> setAt i 1 ax) a [1..(n - 1)]

        b' =
            List.foldl (\i bx -> setAt i 4 bx) b [1..(n - 1)]

        c' =
            List.foldl (\i cx -> setAt i 1 cx) c [1..(n - 1)]

        r' =
            List.foldl (\i rx -> setAt i (4 * (gk i) + 2 * (gk (i + 1))) rx) r [1..(n - 1)]

        a'' =
            setAt (n - 1) 2 a'

        b'' =
            setAt (n - 1) 7 b'

        c'' =
            setAt (n - 1) 0 c'

        r'' =
            setAt (n - 1) (8 * gk (n - 1) + gk n) r'

        p1 =
            solve a'' b'' c'' r''

        p2 =
            Array.initialize n (always 0)

        p2' =
            List.foldl
                (\i px ->
                    setAt i (2 * gk (i + 1) - (getAt (i + 1) p1)) px
                )
                p2
                [0..(n - 1)]

        p2'' =
            setAt (n - 1) (0.5 * ((gk n) + (getAt (n - 1) p1))) p2'
    in
        { p1 = p1, p2 = p2'' }


solve : Array Float -> Array Float -> Array Float -> Array Float -> Array Float
solve a b c r =
    let
        n =
            Array.length r

        ( b'', r'' ) =
            List.foldl
                (\i ( bx, rx ) ->
                    let
                        m =
                            (getAt i a) / (getAt (i - 1) bx)

                        b' =
                            setAt i ((getAt i bx) - m * (getAt (i - 1) c)) bx

                        r' =
                            setAt i ((getAt i rx) - m * (getAt (i - 1) rx)) rx
                    in
                        ( b', r' )
                )
                ( b, r )
                [1..n]
    in
        let
            p =
                Array.initialize n (always 0)

            p' =
                setAt (n - 1) ((getAt (n - 1) r'') / (getAt (n - 1) b'')) p
        in
            List.foldl (\i p'' -> setAt i ((getAt i r'' - (getAt i c) * (getAt (i + 1) p'')) / (getAt i b'')) p'') p' (List.reverse [0..(n - 2)])
