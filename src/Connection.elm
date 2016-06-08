module Connection exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, negate)
import Math.Vector4 as Vec4 exposing (Vec4, vec4, getX, getY, getZ, getW)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import String
import Array
import Symbol
import SvgUtil exposing (path)

type alias Model =
    { symbols: List (Symbol.Model)
    , width : Int
    , stroke : Color
    }


type Msg
    = NoOp

splineStyle =  "fill:transparent;stroke:red;stroke-width:2"

init : List Symbol -> ( Model, Cmd Msg )
init symbols =
    noFx <| Model symbols 3 Color.black

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp  ->
            noFx model

view : Model -> Svg Msg
view model =
    let paths = List.map (SvgUtil.path splineStyle) (splines model.symbols)
    in Svg.g [] paths 

getAt idx arr = Array.get idx arr |> Maybe.withDefault 0
setAt idx val arr = Array.set idx val arr 

controlPoints: Array Float -> (Vec2, Vec2)
controlPoints k =
    let gk i = getAt i k
        internal a b c r gk i =
            (setAt i 1 a, setAt i 4 b, setAt i 1 c, setAt i <| 4*(gk i) + 2 * gk (i+1)) 

        n = -1 + List.length k

        a = Array.repeat 1 0
        b = Array.repeat 1 2
        c = Array.repeat 1 1
        r = Array.repeat 1 ((gk 0)+2*(gk 1))

        (a',b',c',r') = List.map (internal a b c r gk) [1..(n-1)]

        a'' = setAt (n-1) 2
        b'' = setAt (n-1) 7
        c'' = setAt (n-1) 0
        r'' = setAt (n-1) <| 8*(gk (n-1)) + (gk n)
    in 
        let p1 = tridiag a b c r
            p2 = List.map (\i -> 2 * (gk (i+1)) - (getAt (i+1) p1)) [0..(n-1)]
            p2' = setAt (n-1) (0.5 * ((gk n) + getAt (n-1) p1)) p2
        in (p1,p2)  

splines symbols =
    let poses = List.map .pos symbols
        px = controlPoints (List.map getX poses)
        py = controlPoints (List.map getY poses)
        (x::xs) = symbols
        (ys::y) = symbols
    in List.map (path x py py y) symbols 

translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

sx = toString << getX
sy = toString << getY

sxy vec = (sx vec) ++ "," ++ (sy vec)

-- provided by fredcy (thanks!) 
ds : String -> List number -> String
ds tag values =
    tag ++ " " ++ (String.join " " (List.map toString values)) ++ " "


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list

toSample: Model->List Vec2
toSample model =
    let diff = sub model.to model.from 
    --in [ model.to, add model.to (scale 0.1 diff), sub model.from (scale 0.1 diff), model.from ]
    in [ model.to,  model.from ]

--M10 80 C 40 10, 65 10, 95 80 S 150 150, 180 80
quadraticBezier : Model -> List ( String -> Svg.Attribute b, String )
quadraticBezier model =
    let mid = scale 0.5 <| add model.from model.to
        radius = 1.0
        diff = sub mid model.from
        third = scale 0.3 diff
        n = scale radius <| vec2 (getY diff) -(getX diff)
        p1 = add model.from (add third n)
        p2 = add p1 third
        p3 = add p2 (add third <| Vec2.negate n )
        p4 = sub model.to (add third <| Vec2.negate n )
        path =  ds "M" [ getX model.from, getY model.from ]
                ++ ds "C" [ getX p1, getY p1 ] ++ ","
                ++ ds "" [ getX p2, getY p2 ] ++ ","
                ++ ds "" [ getX p3, getY p3 ] ++ " "
                ++ ds "S" [ getX p4, getY p4 ] ++ ", "
                ++ ds "" [ getX model.to, getY model.to ]
                |> Debug.log "dVal"
--    in [ (,) SA.d <| "M" ++ sxy model.from ++ " Q" ++ sxy ctrlPoint ++ " " ++ sxy model.to ]
--                ++ ds "A" [ radius, radius, 0, 0, 1, getX model.to, getY model.to ]
    in [ (,) SA.d path]

connection : Model -> List ( String -> Svg.Attribute b, String )
connection model =
    [ ( SA.x1, toString <| getX model.from )
    , ( SA.y1,  toString <| getY model.from)
    , ( SA.x2,  toString <| getX model.to)
    , ( SA.y2,  toString <| getY model.to)
    , ( SA.stroke, colorToHex model.stroke )
    , ( SA.width, toString model.width )
    , ( SA.fill, "black" )
    ]

        --++ ( attrs <| quadraticBezier model )) []
