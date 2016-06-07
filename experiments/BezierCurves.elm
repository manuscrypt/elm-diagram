import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import VirtualDom exposing (Node)
import String

type alias Sample = List Vec2

samples : List Sample
samples =
  [ [ ( vec2 10 10 ), ( vec2 20 10 ), ( vec2 40 10 ), ( vec2 50 10 ) ]
  , [ ( vec2 10 50 ), ( vec2 20 55 ), ( vec2 40 50 ), ( vec2 50 50 ) ]
  , [ ( vec2 10 100 ), ( vec2 20 110 ), ( vec2 40 100 ), ( vec2 50 100 ) ]
  , [ ( vec2 10 150 ), ( vec2 20 170 ), ( vec2 40 150 ), ( vec2 50 150 ) ]
  , [ ( vec2 10 200 ), ( vec2 20 230 ), ( vec2 40 200 ), ( vec2 50 200 ) ]
  , [ ( vec2 110 10 ), ( vec2 112 10 ), ( vec2 120 10 ), ( vec2 150 10 ) ]
  , [ ( vec2 110 50 ), ( vec2 112 55 ), ( vec2 120 50 ), ( vec2 150 50 ) ]
  , [ ( vec2 110 100 ), ( vec2 112 110 ), ( vec2 120 100 ), ( vec2 150 100 ) ]
  , [ ( vec2 110 150 ), ( vec2 112 170 ), ( vec2 120 150 ), ( vec2 150 150 ) ]
  , [ ( vec2 110 200 ), ( vec2 112 230 ), ( vec2 120 200 ), ( vec2 150 200 ) ]
  ]

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

sampleToSvgPoints : Sample -> String
sampleToSvgPoints sample =
  let head = vecToStr ( Maybe.withDefault ( vec2 0 0 ) ( List.head sample ) )
  in let tails = String.concat ( List.intersperse ", " ( List.map vecToStr ( Maybe.withDefault [] ( List.tail sample ) ) ) )
  --in let tails = List.tail sample
  in
    "M " ++ head ++ " C " ++ tails

sampleToSvg : Sample -> List ( VirtualDom.Node a )
sampleToSvg sample =
  ( List.map vecToCircle sample )
  ++ [ ( Svg.path [ d ( sampleToSvgPoints sample ), stroke "black", fill "transparent" ] [] ) ]

samplesToSvg : List Sample -> List ( VirtualDom.Node a )
samplesToSvg samples =
  List.concat ( List.map sampleToSvg samples )


--main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      ( samplesToSvg samples )
