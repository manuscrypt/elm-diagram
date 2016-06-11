
module Spline exposing (..)

import List
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, negate)
import Array exposing (Array,fromList)
import Array.Extra as Array
import SvgUtil exposing (..)
import ThomasAlgorithm exposing (tridiag)
import Util exposing (..)

type alias VecN =
    Array Float


getXs : List Vec2 -> VecN
getXs points =
    Array.map getX <| Array.fromList points


getYs : List Vec2 -> VecN
getYs points =
    Array.map getY <| Array.fromList points


computeControlPoints : VecN -> {p1: VecN, p2: VecN}
computeControlPoints k =
    let
        n =
            (Array.length k) - 1

        gk i =
            Array.getUnsafe i k

        a =
            fromList [ 0 ]

        b =
            fromList [ 2 ]

        c =
            fromList [ 1 ]

        r =
            fromList [ gk 0 + 2 * (gk 1) ]

        a' =
            Array.append a <| Array.repeat (n - 2) 1

        b' =
            Array.append b <| Array.repeat (n - 2) 4

        c' =
            Array.append c <| Array.repeat (n - 2) 1

        r' =
            Array.append r <| Array.initialize (n - 2) (\i -> 4 * (gk i) + 2 * (gk (i + 1)))

        a'' =
            Debug.log "a" <| Array.push 2 a'

        b'' =
            Debug.log "b" <| Array.push 7 b'

        c'' =
            Debug.log "c" <| Array.push 0 c'

        r'' =
            Debug.log "r" <| Array.push (8 * gk (n - 1) + gk n) r'

        tri = Debug.log "tri" <| tridiag a'' b'' c'' r''

        p1 = Maybe.withDefault (Array.fromList [20,20]) <| Array.get 0 tri

        p2' = Array.initialize (n-2) (\i -> (2 * gk (i+1))- (getAt (i+1) p1 ))
        p2'' = Array.push (0.5 * (gk n + (getAt (n-1) p1))) p2'

    in {p1 = p1, p2 = p2''}


splines : List Vec2 -> List String
splines points =
    let
        xs =
            getXs points

        ys =
            getYs points

        px = computeControlPoints xs

        py =
            computeControlPoints ys

        fst = List.head points
        lst = List.drop (List.length points - 1) points
        
    in
         List.map (toPath xs ys px py) [0..(List.length points - 1)] 


toPath : VecN->VecN->{p1: VecN, p2: VecN}->{p1: VecN, p2: VecN}->Int->String
toPath xs ys px py idx =
  let from = vec2 (getAt idx xs) (getAt idx ys)
      p1' = vec2 (getAt idx <| px.p1) (getAt idx  <| py.p1)  
      p2' = vec2 (getAt idx px.p2) (getAt idx <| py.p2)
      to = vec2 (getAt (idx+1) xs) (getAt (idx+1) ys)  
  in "M " ++ sp from ++ " C " ++ sp p1' ++ " " ++ sp p2' ++ " " ++ sp to

 



-- /*solves Ax=b with the Thomas algorithm (from Wikipedia)*/
-- for (i = 1; i < n; i++)
-- {
--  m = a[i]/b[i-1];
--  b[i] = b[i] - m * c[i - 1];
--  r[i] = r[i] - m*r[i-1];
-- }
-- p1[n-1] = r[n-1]/b[n-1];
-- for (i = n - 2; i >= 0; --i)
--  p1[i] = (r[i] - c[i] * p1[i+1]) / b[i];
-- /*we have p1, now compute p2*/
-- for (i=0;i<n-1;i++)
--  p2[i]=2*K[i+1]-p1[i+1];
-- p2[n-1]=0.5*(K[n]+p1[n-1]);
-- return {p1:p1, p2:p2};
