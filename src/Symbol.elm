module Symbol exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Extra.Cmd exposing (noFx)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)

type Shape
    = Circle
    | Rect

type alias Model =
    { id : Int
    , label : String
    , shape : Shape
    , color : Color
    , size : Vec2
    , pos : Vec2
    }


type Msg
    = Move Vec2
    | SetColor Color
    | Resize Vec2
    | NoOp


init : Int -> String -> Color -> Vec2 -> Vec2 -> ( Model, Cmd Msg )
init id label color size pos =
    (Model id label Circle color size pos) ! []

update : Msg -> Model -> ( Model, Cmd Msg ) 
update msg model =
    case msg of
        Move pos ->
            noFx { model | pos = pos }

        SetColor clr ->
            noFx { model | color = clr }

        Resize size ->
            noFx { model | size = size }

        NoOp ->
            noFx model


view : Model -> Svg Msg
view model =
    let
        label = Svg.text'
                [ SA.x "0"
                , SA.y "25"
                , SA.textAnchor "middle"
                , SA.alignmentBaseline "middle"
                , SA.style "font-weight:bold; font-size:15; font-family: Courier; fill: black;"
                ] [ Svg.text model.label ]
    in 
        Svg.g
            [ SA.transform <| translate model.pos
              --, SE.onMouseDown (Drag.start DragMsg Drag.OnDragStart)
            ]
            [ toSvg model
            , label
            ]


toSvg : Model-> Svg Msg
toSvg model =
    case model.shape of
        Circle ->
            Svg.circle (attrs <| circle model) []

        Rect ->
            Svg.rect (attrs <| rect model) []


translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


circle : Model-> List ( String -> Svg.Attribute b, String )
circle model =
    [ ( SA.cx, "0" )
    , ( SA.cy, "0" )
    , ( SA.r, toString <| getX model.size )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    ]


rect : Model-> List ( String -> Svg.Attribute b, String )
rect model =
    [ ( SA.x, "0" )
    , ( SA.y, "0" )
    , ( SA.width, toString <| getX model.size )
    , ( SA.height, toString <| getY model.size )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    ]


move : { a | pos : Vec2 } -> Float -> { a | pos : Vec2 }
move model t =
    { model | pos = Vec2.add model.pos (Vec2.scale t (vec2 1 1)) }
