module Dynamic.SvgVisualization exposing (..)

import VirtualDom exposing (Node)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Extra.Svg as SvgUtils exposing (Stroke, bezierLineWithDirection, arrow)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Color exposing (Color)


-- VIEW


connectionStroke : Stroke
connectionStroke =
    Stroke Color.black 1


connection : Vec2 -> Vec2 -> List (VirtualDom.Node a)
connection posA posB =
    [ (bezierLineWithDirection (Math.Vector2.add posA (vec2 0 20))
        (vec2 0 40)
        (vec2 0 -60)
        (Math.Vector2.add posB (vec2 0 -20))
        connectionStroke
      )
    , (arrow (Math.Vector2.add posB (vec2 0 -20))
        (vec2 0 -1)
        connectionStroke
        Color.blue
      )
    ]


nodeStroke : Stroke
nodeStroke =
    Stroke Color.darkBlue 1


node : Vec2 -> String -> List (VirtualDom.Node a)
node pos caption =
    [ (SvgUtils.circle pos 20 nodeStroke Color.lightBlue)
    , (Svg.text'
        [ SA.x (toString (getX pos))
        , SA.y (toString ((getY pos) + 2))
        , SA.textAnchor "middle"
        , SA.alignmentBaseline "middle"
        , SA.style "font-size: 12px; font-family: Arial,Helvetica; fill: black;"
        ]
        [ Svg.text caption ]
      )
    ]


gridStroke : Stroke
gridStroke =
    Stroke Color.lightBlue 0.15


gridLine : Float -> Float -> Float -> Float -> VirtualDom.Node a
gridLine x1 y1 x2 y2 =
    Svg.line
        ([ SA.x1 (toString x1)
         , SA.y1 (toString y1)
         , SA.x2 (toString x2)
         , SA.y2 (toString y2)
         ]
            ++ (SvgUtils.strokeToSA gridStroke)
        )
        []


gridLinesY : Float -> Float -> Float -> Float -> Float -> List (VirtualDom.Node a)
gridLinesY x stepSize maxX y1 y2 =
    if (x < maxX) then
        [ gridLine x y1 x y2 ]
            ++ (gridLinesY (x + stepSize) stepSize maxX y1 y2)
    else
        []


gridLinesX : Float -> Float -> Float -> Float -> Float -> List (VirtualDom.Node a)
gridLinesX y stepSize maxY x1 x2 =
    if (y < maxY) then
        [ gridLine x1 y x2 y ]
            ++ (gridLinesX (y + stepSize) stepSize maxY x1 x2)
    else
        []


grid : ( Float, Float, Float, Float ) -> List (VirtualDom.Node a)
grid ( minX, minY, maxX, maxY ) =
    let
        stepSize =
            25
    in
        --[ gridLine minX minY maxX maxY ] ++
        (gridLinesY minX stepSize maxX minY maxY)
            ++ (gridLinesX minY stepSize maxY minX maxX)



{- DynamicLayout.viewbox viewboxMargin model.layout
         stepSize = calcStepSize minY maxY
     in ( drawLinesX ( toFloat <| round ( minX - stepSize ) ) stepSize maxX minY maxY )
     ++ ( drawLinesY ( toFloat <| round ( minY - stepSize ) ) stepSize maxY minX maxX )

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

-}
{-
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
-}
