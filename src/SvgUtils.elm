module SvgUtils exposing (..)

import Svg
import Svg.Attributes as SA
import Html
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import VirtualDom exposing (Node)
import String
import Debug
import Color exposing (Color)
import Color.Convert exposing (colorToHex)

--type Stroke = Transparent | SolidStroke
type alias Stroke =
  { color : Color
  , width : Float
  }

circle : Vec2 -> Float -> Stroke -> Color -> VirtualDom.Node a
circle pos radius stroke fillcolor =
  Svg.circle
    ( [ SA.cx ( vecXtoString pos ), SA.cy ( vecYtoString pos ), SA.r ( toString radius ), SA.fill ( colorToHex fillcolor ) ]
      ++ ( strokeToSA stroke ) )
    []

bezierLineWithDirection : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Stroke -> VirtualDom.Node a
bezierLineWithDirection startPos startDir endDir endPos stroke =
  bezierLineWithControlPoints startPos ( Math.Vector2.add startPos startDir ) ( Math.Vector2.add endPos endDir ) endPos stroke

bezierLineWithControlPoints : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Stroke -> VirtualDom.Node a
bezierLineWithControlPoints startPos startControlPos endControlPos endPos stroke =
  let dstr = (
    "M " ++ ( vecToStringX_Y startPos ) ++
    " C " ++ ( vecToStringX_Y startControlPos ) ++
    ", "  ++ ( vecToStringX_Y endControlPos ) ++
    ", "  ++ ( vecToStringX_Y endPos )
    )
  in Svg.path ( [ SA.d dstr, SA.fill "transparent" ] ++ ( strokeToSA stroke ) ) []

strokeToSA : Stroke ->  List (VirtualDom.Property a)
strokeToSA stroke =
  [ SA.stroke ( colorToHex stroke.color )
  , SA.strokeWidth (toString stroke.width ) ]

vecXtoString : Vec2 -> String
vecXtoString v =
  toString ( getX v )

vecYtoString : Vec2 -> String
vecYtoString v =
  toString ( getY v )

vecToStringX_Y : Vec2 -> String
vecToStringX_Y v =
  ( vecXtoString v ) ++ " " ++ ( vecYtoString v )

-- sample

redStroke : Stroke
redStroke = Stroke Color.red 1
redThinStroke = Stroke Color.red 0.3
blueThinStroke = Stroke Color.blue 0.3

main : Node a
main =
  Svg.svg [ SA.version "1.1", SA.x "0", SA.y "0", SA.viewBox "0 0 323.141 322.95" ]
      [ ( circle ( vec2 100 10 ) 5 redThinStroke Color.yellow )
      , ( circle ( vec2 100 20 ) 15 redStroke Color.yellow  )
      , ( bezierLineWithDirection ( vec2 50 40 ) ( vec2 0 0 ) ( vec2 0 0 ) ( vec2 150 40 ) blueThinStroke )
      , ( bezierLineWithDirection ( vec2 50 50 ) ( vec2 0 10 ) ( vec2 0 -10 ) ( vec2 150 50 ) blueThinStroke )
      , ( bezierLineWithDirection ( vec2 150 50 ) ( vec2 0 10 ) ( vec2 0 -10 ) ( vec2 250 50 ) redThinStroke )
      , ( bezierLineWithDirection ( vec2 50 60 ) ( vec2 0 20 ) ( vec2 0 -20 ) ( vec2 150 60 ) blueThinStroke )
      , ( bezierLineWithDirection ( vec2 150 60 ) ( vec2 0 20 ) ( vec2 0 -20 ) ( vec2 250 60 ) redThinStroke )
      ]
