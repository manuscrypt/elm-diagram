module Connection exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, negate)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import String


type alias Model =
    { from: Vec2
    , to : Vec2
    , width : Int
    , stroke : Color
    }


type Msg
    = NoOp


init : Vec2 -> Vec2 -> ( Model, Cmd Msg )
init from to =
    let f = Debug.log "connect" (from,to)
    in noFx <| Model from to 3 Color.black

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp  ->
            noFx model

view : Model -> Svg Msg
view model =
    Svg.path ([SA.style "fill:transparent;stroke:red;stroke-width:2"] ++ ( attrs <| quadraticBezier model )) []

translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

sx = toString << getX
sy = toString << getY

sxy vec = (sx vec) ++ "," ++ (sy vec)


ds : String -> List number -> String
ds tag values =
    tag ++ " " ++ (String.join " " (List.map toString values)) ++ " "


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


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
