module Connection exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, negate)
import ThomasAlgorithm exposing (tridiag)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import Color exposing (Color)
import String
import Array exposing (Array)
import Symbol

type alias Model =
    { symbols: List (Symbol.Model)
    , width : Int
    , stroke : Color
    }


type Msg
    = NoOp  

splineStyle =  "fill:transparent;stroke:red;stroke-width:2"

init : List Symbol.Model -> ( Model, Cmd Msg )
init symbols =
    noFx <| Model symbols 3 Color.black

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp  ->
            noFx model

view : Model -> Svg Msg
view model =
    let paths = splines model.symbols
    in Svg.g [] paths 

getAt idx arr = Array.get idx arr |> Maybe.withDefault 0
setAt idx val arr = Array.set idx val arr 

internal: (Int->Float)->Int->(Array Float, Array  Float,Array  Float,Array   Float)-> (Array Float,Array   Float,Array  Float,Array   Float)
internal gk i (a, b, c, r) =
    (setAt i 1 a, setAt i 4 b, setAt i 1 c, setAt i (4*(gk i) + 2 * gk (i+1)) r) 

toVec2 p = vec2 (getAt 0 p) (getAt 1 p)

controlPoints: Array Float -> (Vec2, Vec2)
controlPoints k =
    let gk i = getAt i k

        n = -1 + Array.length k

        a = Array.repeat 1 0
        b = Array.repeat 1 2
        c = Array.repeat 1 1
        r = Array.repeat 1 ((gk 0)+2*(gk 1))

        (a',b',c',r') = Array.foldl (internal gk) (a,b,c,r) <| Array.fromList [1..(n-1)]

        a'' = setAt (n-1) 2
        b'' = setAt (n-1) 7
        c'' = setAt (n-1) 0
        r'' = setAt (n-1) <| 8*(gk (n-1)) + (gk n) 
    in 
        let p1 = Maybe.withDefault (Array.repeat n 0) <| List.head <| Array.toList <| tridiag a b c r
            p2 = Array.map (\i -> 2 * (gk (i+1)) - (getAt (i+1) p1)) <| Array.fromList [0..(n-1)]
            p2' = setAt (n-1) (0.5 * ((gk n) + getAt (n-1) p1)) p2
        in (toVec2 p1, toVec2 p2)  


splines : List Symbol.Model -> List (Svg a)
splines symbols =
    let arrSyms = Array.fromList symbols
        poses = Array.map .pos arrSyms 
        px = controlPoints (Array.map getX poses)
        py = controlPoints (Array.map getY poses)
        x = Maybe.withDefault (vec2 0 0 ) <| Array.get 0 poses
        y = Maybe.withDefault (vec2 0 0 ) <| Array.get ((Array.length poses)-1) poses
    in Array.toList <| Array.map (path splineStyle x px py y) arrSyms  


path: String->Vec2->(Vec2,Vec2)->(Vec2,Vec2)->Vec2->Symbol.Model->Svg a
path st p1 (cp1,x) (cp2,y) p2 sym =
  let path = "M " ++  sp p1 ++ " C " ++ (String.join " " <| List.map sp [cp1, cp2, p2])
  in Svg.path [SA.style st, SA.d path] []

translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

s s = toString s
sx = toString << getX
sy = toString << getY
sxy vec = (sx vec) ++ "," ++ (sy vec)
sp vec = (sx vec) ++ " " ++ (sy vec)
