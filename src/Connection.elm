module Connection exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import VirtualDom exposing (Node)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)

import Color exposing (Color)

import Extra.Cmd exposing (noFx)
import Extra.Svg exposing (Stroke,bezierLineWithDirection,arrow)

import Symbol

type alias Model =
    { symbols: List (Symbol.Model)
    , screenSize: Vec2
    , width : Int
    , stroke : Color
    }


type Msg
    = NoOp

splineStyle: String
splineStyle =  "fill:transparent;stroke:red;stroke-width:2;z-index:-1000"

blackStroke = Stroke Color.black 2

init : List Symbol.Model -> Vec2-> ( Model, Cmd Msg )
init symbols size =
    noFx <| Model symbols size 3  Color.black

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp  ->
            noFx model


createEdge : Symbol.Model -> Symbol.Model -> List ( VirtualDom.Node a )
createEdge fromModel toModel =
    [ ( bezierLineWithDirection
        ( Math.Vector2.add fromModel.pos ( vec2 0 20 ) )
        ( vec2 0 40 )
        ( vec2 0 -60 )
        ( Math.Vector2.add toModel.pos ( vec2 0 -20 ) )
        blackStroke
      )
    , ( arrow
        ( Math.Vector2.add toModel.pos ( vec2 0 -20 ) )
        ( vec2 0 -1 )
        blackStroke
        Color.red
      )
    ]

createEdges : List (Symbol.Model) -> (List ( VirtualDom.Node a ) )
createEdges nodes =
    case nodes of
        a::b::_ -> ( createEdge a b ) ++ ( createEdges ( List.drop 1 nodes ) )
        _  -> []

view : Model -> Svg Msg
view model =
    let paths = -- List.map (\p -> Svg.path [SA.d p, SA.style splineStyle][] ) <| splines (List.map .pos model.symbols)
         -- [ ( bezierLineWithDirection ( vec2 210 67.5 ) ( vec2 0 40 ) ( vec2 20 -30 ) ( vec2 145 135 ) blackStroke )
         -- ]
         createEdges model.symbols

    in Svg.g [] paths
