module Visuals.Layout.ViewportCalculator exposing ( fromPositions )

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub)
import List
import String
import Maybe

fromPositions : Float -> List Vec2 -> String
fromPositions margin positions =
  let xs = List.map ( \p -> ( getX p ) ) positions
      ys = List.map ( \p -> ( getY p ) ) positions
      minx = ( Maybe.withDefault 0 <| List.minimum xs ) - margin
      maxx = ( Maybe.withDefault 0 <| List.maximum xs ) + margin
      miny = ( Maybe.withDefault 0 <| List.minimum ys ) - margin
      maxy = ( Maybe.withDefault 0 <| List.maximum ys ) + margin
  in
    ( toString ( minx ) ) ++ " " ++
    ( toString ( miny ) ) ++ " " ++
    ( toString ( maxx - minx ) ) ++ " " ++
    ( toString ( maxy - miny ) )
