module Visuals.Diagram.Layout exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, length)
import Time exposing (Time)
import Visuals.Diagram.Node as Node exposing (zero)
import IntDict exposing (IntDict)
import String


type alias Model n =
    List (Node.Model n)


viewBox : Model n -> String
viewBox model =
    let
        xs =
            List.map (getX << .pos) model

        ys =
            List.map (getY << .pos) model

        minx =
            ( Maybe.withDefault 0 <| List.minimum xs ) - 100

        maxx =
            ( Maybe.withDefault 0 <| List.maximum xs ) + 100

        miny =
            ( Maybe.withDefault 0 <| List.minimum ys ) - 100

        maxy =
            ( Maybe.withDefault 0 <| List.maximum ys ) + 100
    in
        String.join " " <| List.map toString [ minx, miny, (maxx - minx), (maxy - miny) ]


animate : Time -> List ( Int, Int ) -> Model n -> Model n
animate dt edges model =
    let
        timeInSecs =
            dt / 1000
    in
        List.map (allForcesForOne timeInSecs edges model) model
            |> fiddle timeInSecs
            |> List.map (Node.forward timeInSecs)


fiddle : Time -> Model n -> Model n
fiddle time model =
    -- (\index force velocity ->
    --     -- v = a * t + v0
    --     add (scale (dt * 0.0001) force)
    --         (scale (0.999) velocity)
    -- )
    model



--------------------


allForcesForOne : Time -> List ( Int, Int ) -> Model n -> Node.Model n -> Node.Model n
allForcesForOne dt edges model body =
    let
        others =
            List.filter (\n -> n.id /= body.id) model
    in
        let
            forces =
                (calcForcesOneVsAll dt others body)
                    ++ (calcForcesOneOnOne dt edges others body)
                    ++ (calcForcesForOne dt model body)
        in
            { body | force = List.foldl add body.force forces }


calcForcesOneVsAll : Time -> List (Node.Model n) -> Node.Model n -> List Vec2
calcForcesOneVsAll dt others body =
    List.filterMap (calcForcesForNoIntersection dt body) others

getPosOf : Int -> List ( Node.Model n ) -> Vec2
getPosOf id nodes =
    let ff = List.filter ( \n -> n.id == id ) nodes
    in case ff of
      [a] -> a.pos
      _ -> Debug.crash "node not found"

calcForcesOneOnOne : Time -> List ( Int, Int ) -> List (Node.Model n) -> Node.Model n -> List Vec2
calcForcesOneOnOne dt edges others body =
    let incEdges = List.filter ( \( fromId, toId ) -> fromId == body.id ) edges
        incForces = List.map ( \( fromId, toId ) ->
                      let toPos = getPosOf toId others
                          fx = ( getX body.pos )
                          fy = ( getY body.pos )
                          tx = ( getX toPos )
                          ty = ( getY toPos )
                      in
                        vec2
                          (
                            if( tx < tx )then 5 else if( fx > tx ) then -5 else 0
                          )
                          (  if( fy <= ty + 50 )then
                                15 --( 0.51 * ( fy - 0 - ty ) )
                             else
                                0
                          )
                    ) incEdges
        --outEdges = List.filter ( \( fromId, toId ) -> toId == body.id )
    in
      --List.concat <|
        --List.map ( \( fromId, toId ) -> calcForcesOneOnOneFor dt body) edges
        incForces


calcForcesForOne : Time -> Model n -> Node.Model n -> List Vec2
calcForcesForOne dt model body =
    [ scale -0.4 body.vel ]



-------------------------------------------------------------


calcForcesForNoIntersection : Time -> Node.Model n -> Node.Model n -> Maybe Vec2
calcForcesForNoIntersection dt elemA elemB =
    let
        minDistance =
            50

        diff =
            (sub elemA.pos elemB.pos)

        dist =
            (length diff)
    in
        if (dist > minDistance) then
            Nothing
        else
            Just diff



-- let
--     norm =
--         if (dist < 0.0001) then
--             (vec2 1 1)
--         else
--             (normalize diff)
--
--     factor =
--         0.01 + 0.1 * (minDistance - dist)
-- in
--     Just <| scale factor norm


calcForcesOneOnOneFor : Time -> Node.Model n -> Node.Model n -> Vec2
calcForcesOneOnOneFor dt nodeA nodeB =
    zero



-- let
--     -- a und b sollten gleiches x haben
--     -- b soll 80 unter a sein
--     target =
--         (vec2 0 80)
--
--     diff =
--         sub nodeB.pos nodeA.pos
-- in
--     sub diff target
