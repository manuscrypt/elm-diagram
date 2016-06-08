module SvgUtil exposing (sampleToSvg,path)

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

s: number->String
s s = toString s

path: String->(number,number)->(number,number)->(number,number)->(number,number)->Svg a
path st (x1, y1) (px1, py1) (px2, py2) (x2, y2) =
  let path = "M " ++  (String.join " " <| List.map s [x1,y1]) ++ " C " ++ (String.join " " <| List.map s [px1,py1,px2,py2,x2,y2])
  in Svg.path [SA.style st, SA.d path] []

