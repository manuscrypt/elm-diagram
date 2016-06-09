module BezierCurves exposing ( nothing )

import Svg exposing (..)
import Svg.Attributes exposing (..)
-- <<<<<<< HEAD
-- import Math.Vector2 exposing (Vec2, vec2, getX, getY)
-- import VirtualDom exposing (Node)
-- import String
-- import SvgUtil

-- type alias Sample = List Vec2

-- samples : List Sample
-- samples =
--   [ [ ( vec2 10 10 ), ( vec2 20 10 ), ( vec2 40 10 ), ( vec2 50 10 ) ]
--   , [ ( vec2 10 50 ), ( vec2 20 55 ), ( vec2 40 50 ), ( vec2 50 50 ) ]
--   , [ ( vec2 10 100 ), ( vec2 20 110 ), ( vec2 40 100 ), ( vec2 50 100 ) ]
--   , [ ( vec2 10 150 ), ( vec2 20 170 ), ( vec2 40 150 ), ( vec2 50 150 ) ]
--   , [ ( vec2 10 200 ), ( vec2 20 230 ), ( vec2 40 200 ), ( vec2 50 200 ) ]
--   , [ ( vec2 110 10 ), ( vec2 112 10 ), ( vec2 120 10 ), ( vec2 150 10 ) ]
--   , [ ( vec2 110 50 ), ( vec2 112 55 ), ( vec2 120 50 ), ( vec2 150 50 ) ]
--   , [ ( vec2 110 100 ), ( vec2 112 110 ), ( vec2 120 100 ), ( vec2 150 100 ) ]
--   , [ ( vec2 110 150 ), ( vec2 112 170 ), ( vec2 120 150 ), ( vec2 150 150 ) ]
--   , [ ( vec2 110 200 ), ( vec2 112 230 ), ( vec2 120 200 ), ( vec2 150 200 ) ]
--   , [ ( vec2 110 200 ), ( vec2 112 230 ), ( vec2 120 200 ), ( vec2 150 250 ) ]
--   ]


-- samplesToSvg : List Sample -> List ( VirtualDom.Node a )
-- samplesToSvg samples =
--   List.concat ( List.map SvgUtil.sampleToSvg samples )
-- =======
import SvgUtils
import Html exposing (Html)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction )
import VirtualDom exposing (Node)
import String
import Color

nothing =
  "Nothing"

points : List Vec2
points =
  [ ( vec2 20 20 )
  , ( vec2 50 20 )
  , ( vec2 90 20 )
  , ( vec2 90 50 )
  , ( vec2 30 50 )
  , ( vec2 30 80 )
  , ( vec2 80 80 )
  , ( vec2 23 110 )
  , ( vec2 20 50 )
  , ( vec2 20 32 )
  ]

redThinStroke = SvgUtils.Stroke Color.red 0.3
blackStroke = SvgUtils.Stroke Color.black 1
blueThinStroke = SvgUtils.Stroke Color.blue 0.3

pointsAsCircles : List ( VirtualDom.Node a )
pointsAsCircles  =
  List.map ( \pos -> ( SvgUtils.circle pos 1 blackStroke ) ) points

listLast list = List.head list -- ( List.reverse list )

directionPoints : List ( VirtualDom.Node a )
directionPoints =
  case List.head points of
    Nothing -> []
    Just first ->
      case listLast points of
        Nothing -> []
        Just last ->
          nextSegment ( [ last ] ++ points ++ [ first ] )

schwingFaktor = 10.0
directionTo : Vec2 -> Vec2 -> Vec2
directionTo from to =
  Math.Vector2.scale schwingFaktor ( Math.Vector2.direction to from )
-->>>>>>> 1572316ce21e1da14adc8a0ef3cdd88c4ba4d8df

nextSegment : List Vec2 -> List ( VirtualDom.Node a )
nextSegment p12345 =
  case List.head points of
    Nothing -> []
    Just p1 ->
      let p2345 = List.drop 1 p12345
      in case List.head p2345 of
        Nothing -> []
        Just p2 ->
          let p345 = List.drop 1 p2345
          in case List.head p345 of
            Nothing -> []
            Just p3 ->
              let p45 = List.drop 1 p345
              in case List.head p45 of
                Nothing -> []
                Just p4 ->
                  let d21 = ( directionTo p1 p2 )
                      d34 = ( directionTo p4 p3 )
              in
                [ ( SvgUtils.circle ( add p2 d21 ) 1 redThinStroke )
                , ( SvgUtils.circle ( add p3 d34 ) 1 redThinStroke )
                , ( SvgUtils.bezierLineWithDirection p2 d21 d34 p3 blueThinStroke )
                ]
                ++ ( nextSegment p2345 )

main : Node a
main =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 323.141 322.95" ]
      ( pointsAsCircles ++ directionPoints )
