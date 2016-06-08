import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import VirtualDom exposing (Node)
import String
import SvgUtil

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
  , [ ( vec2 110 200 ), ( vec2 112 230 ), ( vec2 120 200 ), ( vec2 150 250 ) ]
  ]


samplesToSvg : List Sample -> List ( VirtualDom.Node a )
samplesToSvg samples =
  List.concat ( List.map SvgUtil.sampleToSvg samples )


--main : Html
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      ( samplesToSvg samples )
