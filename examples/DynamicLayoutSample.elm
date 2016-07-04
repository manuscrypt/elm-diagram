module DynamicLayoutSample exposing (..)

import VirtualDom exposing (Node)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Extra.Svg as SvgUtils exposing (Stroke,bezierLineWithDirection,arrow)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction )
import Html exposing (Html)
import Html.App as Html
import Time exposing (Time, second)
import Color exposing ( Color )
import DynamicLayout
import String

--  SAMPLE

type alias SampleNode =
    { name : String
    }

sampleNode0 = { name = "n0" }
sampleNode1 = { name = "n1" }
sampleNode2 = { name = "n2" }
sampleNode3 = { name = "n3" }
sampleNode4 = { name = "n4" }
sampleNode5 = { name = "n5" }
sampleNode6 = { name = "n6" }
sampleNode7 = { name = "n7" }
sampleNode8 = { name = "n8" }

sampleNodes = 
  [ sampleNode0
  , sampleNode1
  , sampleNode2
  , sampleNode3
  , sampleNode4
  , sampleNode5
  , sampleNode6    
  , sampleNode7    
  , sampleNode8    
  ] 
  ++ List.map ( \nr -> { name = "x" ++ ( toString nr ) } ) [10..15]

sampleConnections = 
  [ ( sampleNode0, sampleNode3 )
  , ( sampleNode1, sampleNode3 )
  , ( sampleNode2, sampleNode5 )
  , ( sampleNode3, sampleNode4 )
  , ( sampleNode3, sampleNode5 )
  , ( sampleNode4, sampleNode6 )
  , ( sampleNode5, sampleNode7 )
  , ( sampleNode5, sampleNode8 )
  , ( { name = "x15" }, { name = "x10" })
  , ( { name = "x10" }, { name = "x11" })
  , ( { name = "x11" }, { name = "x12" })
  , ( { name = "x12" }, { name = "x13" })
  , ( { name = "x13" }, { name = "x14" })
  , ( { name = "x14" }, { name = "x15" })
  ]

sampleLayout =
  DynamicLayout.addForEachRule ( DynamicLayout.noIntersection 100 )
  <| DynamicLayout.addForOneRule ( DynamicLayout.snapToGrid 100 )  
  <| DynamicLayout.addForTwoRule sampleNode6 ( DynamicLayout.hasSameY 0.050 ) sampleNode7    
  <| DynamicLayout.addForTwoRule sampleNode6 ( DynamicLayout.hasSameY 0.050 ) sampleNode8    
  <| DynamicLayout.addForTwoRule sampleNode7 ( DynamicLayout.hasSameY 0.050 ) sampleNode8
  <| makeDepsSameY
  <| ( List.foldl makeConn (
    List.foldl DynamicLayout.addNode DynamicLayout.empty sampleNodes
  ) sampleConnections )

makeConn : ( SampleNode, SampleNode ) -> ( DynamicLayout.Model SampleNode )  -> ( DynamicLayout.Model SampleNode )
makeConn ( nodeA, nodeB ) layout = 
  DynamicLayout.addForTwoRule nodeA ( DynamicLayout.isAbove 50 ) nodeB
  <| DynamicLayout.addForTwoRule nodeA ( DynamicLayout.hasSameX 0.05 ) nodeB layout

makeDepsSameYforAB : ( SampleNode, SampleNode ) -> ( DynamicLayout.Model SampleNode )  -> ( DynamicLayout.Model SampleNode )
makeDepsSameYforAB ( a, b ) layout =
  if( a == b )then layout else
  DynamicLayout.addForTwoRule a ( DynamicLayout.hasSameY 0.05 ) b layout

zip : List a -> List a -> List ( a, a )
zip la lb =
  List.concat <| List.map ( \aa -> List.map ( \bb -> ( aa, bb ) ) lb ) la 

makeDepsSameYfor : SampleNode -> ( DynamicLayout.Model SampleNode )  -> ( DynamicLayout.Model SampleNode )
makeDepsSameYfor parent layout =
  let relcons =  List.filter ( \( a, b ) -> ( b == parent ) ) sampleConnections
      relchs =   List.map ( \( a, b ) -> a ) relcons
      zipped = zip relchs relchs -- List.map ( \a -> List.map ( \b -> ( a, b ) ) relchs ) relchs 
  in List.foldr makeDepsSameYforAB layout zipped

makeDepsSameY : ( DynamicLayout.Model SampleNode )  -> ( DynamicLayout.Model SampleNode )
makeDepsSameY layout =
  List.foldr makeDepsSameYfor layout sampleNodes

-- MODEL

type alias Model =
  { nodes : List SampleNode
  , layout : DynamicLayout.Model SampleNode
  , actTime : Time
  , startTime : Time
  , secsSinseStart : Int
  }

init : ( Model, Cmd Msg )
init = ( { nodes = sampleNodes
          , layout = sampleLayout 
          , actTime = 0
          , startTime = 0
          , secsSinseStart = 0
          }, Cmd.none )

addNode node model =
  { model
  | nodes = model.nodes ++ [ node ]
  , layout = DynamicLayout.addNode node model.layout 
  }

sample : (Model, Cmd Msg)
sample = 
  (  ( fst init ) 
    , Cmd.none )

-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      let
        newStartTime = if model.startTime == 0 then newTime else model.startTime
        timeSinceStart = newTime - newStartTime
        newSecsSinseStart = ( ( round ( ( Time.inMinutes timeSinceStart ) * 100 ) ) % 17 )
      in
      ( { model
        | actTime = newTime
        , startTime = newStartTime
        , secsSinseStart = newSecsSinseStart
        , layout = DynamicLayout.animate model.layout
        } , Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.millisecond Tick

-- VIEW

viewboxMargin = 50 

calcStepSize : Float -> Float -> Float
calcStepSize min max = 
  let d = ( max - min )
  in if( d < 10 ) then 1
  else if( d < 100 ) then 10
  else  100

drawLinesX : Float -> Float -> Float -> Float -> Float -> List ( VirtualDom.Node a )
drawLinesX x stepSize maxX minY maxY =
  [ SvgUtils.bezierLineWithDirection (vec2 x minY ) (vec2 -1 1) (vec2 1 -1) (vec2 x maxY ) ( Stroke Color.blue 0.3 )
  ] ++ if( x + stepSize < maxX )then 
    ( drawLinesX ( x + stepSize ) stepSize maxX minY maxY )
    else []

drawLinesY : Float -> Float -> Float -> Float -> Float -> List ( VirtualDom.Node a )
drawLinesY y stepSize maxY minX maxX =
  [ SvgUtils.bezierLineWithDirection (vec2 minX y ) (vec2 0 0) (vec2 0 0) (vec2 maxX y ) ( Stroke Color.blue 0.3 )
  ] ++ if( y + stepSize < maxY )then 
    ( drawLinesY ( y + stepSize ) stepSize maxY minX maxX )
    else []


viewSvgGrid : Model -> List ( VirtualDom.Node a )
viewSvgGrid model =
  let ( minX, minY, maxX, maxY ) = DynamicLayout.viewbox viewboxMargin model.layout
      stepSize = calcStepSize minY maxY
  in ( drawLinesX ( toFloat <| round ( minX - stepSize ) ) stepSize maxX minY maxY )
  ++ ( drawLinesY ( toFloat <| round ( minY - stepSize ) ) stepSize maxY minX maxX )

viewSvgConnection : SampleNode -> SampleNode -> Model -> List ( VirtualDom.Node a ) 
viewSvgConnection nodeA nodeB model =
  let posA = DynamicLayout.positionOfNode nodeA model.layout
      posB = DynamicLayout.positionOfNode nodeB model.layout
  in
      [ (bezierLineWithDirection (Math.Vector2.add posA (vec2 0 20))
        (vec2 0 40)
        (vec2 0 -60)
        (Math.Vector2.add posB (vec2 0 -20))
        ( Stroke Color.brown 1 )
      )
    , (arrow (Math.Vector2.add posB (vec2 0 -20))
        (vec2 0 -1)
        ( Stroke Color.brown 1 )
        Color.red
      )
    ]


viewSvgNodes : Model -> List ( VirtualDom.Node a )
viewSvgNodes model =
  ( viewSvgGrid model ) 
  ++ ( List.concat <| ( ( List.map ( \( nodeA, nodeB ) -> viewSvgConnection nodeA nodeB model ) sampleConnections ) ) )
  ++ 
  (
  List.concat <| List.map ( \node ->
    let pos = DynamicLayout.positionOfNode node model.layout
    in [ SvgUtils.circle pos 20 ( Stroke Color.black 2 ) Color.lightBlue
       , Svg.text' [ SA.x ( toString ( getX pos ) )
                   , SA.y ( toString ( getY pos ) )
                   , SA.textAnchor "middle"
                   , SA.alignmentBaseline "middle"
                   , SA.style "font-weight:bold; font-size:15; font-family: Courier; fill: black;"
                   ] [ Svg.text node.name ] 
             ]  
           ) model.nodes  )
           
view : Model -> Html Msg
view model =
    Html.div [  ]
      [ svg 
          [ version "1.1"
          , x "0", y "0",  SA.width "600", SA.height "500"
          , viewBox ( DynamicLayout.viewboxAsString viewboxMargin model.layout ) 
          ]
          ( viewSvgNodes model )  
      , text ( ( toString model.actTime ) ++ "-" ++ ( toString model.startTime ) ++ "=" ++ ( toString model.secsSinseStart ) )
      ]

-- MAIN


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
