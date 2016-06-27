module Extra.Svg exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Color exposing (Color)
import Html
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Extra.MathVector2
import VirtualDom exposing (Node)
import String
import Debug
import Color.Convert exposing (colorToHex)

--type Stroke = Transparent | SolidStroke
type alias Stroke =
  { color : Color
  , width : Float
  }

arrow targetPos unnormalizedDirection stroke fillcolor =
    let fromdir = Math.Vector2.normalize ( unnormalizedDirection )
        startPos = Math.Vector2.add targetPos ( Math.Vector2.scale 15 fromdir )
        rightdir = ( Math.Vector2.scale 5 ( Extra.MathVector2.rotate fromdir 90 ) )
        leftdir = Math.Vector2.scale  -1 rightdir
    in
    let dstr = (
      "M " ++ ( vecToStringX_Y targetPos )
      ++ ", " ++ ( vecToStringX_Y ( Math.Vector2.add startPos rightdir ) )
      ++ ", " ++ ( vecToStringX_Y ( Math.Vector2.add startPos leftdir ) )
      ++ ", " ++ ( vecToStringX_Y targetPos )
      )
    in Svg.path ( [ SA.d dstr, SA.fill ( colorToHex fillcolor ) ] ++ ( strokeToSA stroke ) ) []


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

getXs v =
  toString ( getX v )

getYs v =
  toString ( getY v )

vecToStr : Vec2 -> String
vecToStr v =
  ( getXs v ) ++ " " ++ ( getYs v )

vecToCircle : Vec2 -> VirtualDom.Node a
vecToCircle v =
  Svg.circle [ SA.cx ( getXs v ), SA.cy ( getYs v ), SA.r "1", SA.fill "transparent", SA.stroke "red" ] []

sampleToSvgPoints : List Vec2 -> String
sampleToSvgPoints sample =
  let head = vecToStr ( Maybe.withDefault ( vec2 0 0 ) ( List.head sample ) )
  in let tails = String.concat ( List.intersperse ", " ( List.map vecToStr ( Maybe.withDefault [] ( List.tail sample ) ) ) )
  --in let tails = List.tail sample
  in
    "M " ++ head ++ " C " ++ tails

sampleToSvg : List Vec2 -> List ( VirtualDom.Node a )
sampleToSvg sample =
  ( List.map vecToCircle sample )
  ++ [ ( Svg.path [ SA.d ( sampleToSvgPoints sample ), SA.stroke "black", SA.fill "transparent" ] [] ) ]


path: String->Vec2->(Vec2,Vec2)->(Vec2,Vec2)->Vec2->()->Svg a
path st p1 (cp1,x) (cp2,y) p2 _ =
  let path = "M " ++  sp p1 ++ " C " ++ (String.join " " <| List.map sp [cp1, cp2, p2])
  in Svg.path [SA.style st, SA.d path] []

translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

s:Vec2->String
s s = toString s

sxy:Vec2->String
sxy vec = (sx vec) ++ "," ++ (sy vec)

sp:Vec2->String
sp vec = (sx vec) ++ " " ++ (sy vec)

sx:Vec2->String
sx = toString << getX
sy:Vec2->String
sy = toString << getY

vecToSvgPos : Vec2 -> String
vecToSvgPos vec =
    (toString <| getX vec) ++ " " ++ (toString <| getY vec)

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
