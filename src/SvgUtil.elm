module SvgUtil exposing (..)

import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import VirtualDom exposing (Node)
import String


getXs v =
  toString ( getX v )

getYs v =
  toString ( getY v )

vecToStr : Vec2 -> String
vecToStr v =
  ( getXs v ) ++ " " ++ ( getYs v )

vecToCircle : Vec2 -> VirtualDom.Node a
vecToCircle v =
  Svg.circle [ cx ( getXs v ), cy ( getYs v ), r "1", fill "transparent", stroke "red" ] []

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
  ++ [ ( Svg.path [ d ( sampleToSvgPoints sample ), stroke "black", fill "transparent" ] [] ) ]


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
