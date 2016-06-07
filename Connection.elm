module Connection exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY, add, scale)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)


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
    Svg.path ( attrs <| quadraticBezier model ) []

translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

sx = toString getX
sy = toString getY

sxy vec = (sx vec) ++ "," ++ sy vec


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list

quadraticBezier : Model -> List ( String -> Svg.Attribute b, String )
quadraticBezier model =
    let ctrlPoint = scale 0.5 <| add model.from model.to
    in [ (,) SA.d <| "M" ++ sxy model.from ++ " Q" ++ sxy ctrlPoint ++ " " ++ sxy model.to ] 

connection : Model -> List ( String -> Svg.Attribute b, String )
connection model =
    [ ( SA.x1, toString <| getX model.from )
    , ( SA.y1,  toString <| getY model.from)
    , ( SA.x2,  toString <| getX model.to)
    , ( SA.y2,  toString <| getY model.to)
    , ( SA.stroke, colorToHex model.stroke )
    , ( SA.width, toString model.width )
    ]

