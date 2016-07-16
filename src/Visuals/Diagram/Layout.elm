module Visuals.Diagram.Layout exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub)
import Time exposing (Time)
import Visuals.Diagram.Node as Node
import IntDict exposing (IntDict)


type alias Model n =
    List (Node.Model n)


animate : Time -> Model n -> Model n
animate dt model =
    let
        timeInSecs =
            dt / 1000
    in
        List.map (allForcesForOne timeInSecs model) model
            |> fiddle timeInSecs
            |> List.map (Node.forward timeInSecs)


fiddle : Time -> Model n -> Model n
fiddle time model =
    -- (\index force velocity ->
    --     -- v = a * t + v0
    --     Vec2.add (Vec2.scale (dt * 0.0001) force)
    --         (Vec2.scale (0.999) velocity)
    -- )
    model



--------------------


allForcesForOne : Time -> Model n -> Node.Model n -> Node.Model n
allForcesForOne dt model body =
    let
        others =
            List.filter (\n -> n.id /= body.id) model
    in
        let
            forces =
                (calcForcesOneVsAll dt others body)
                    ++ (calcForcesOneOnOne dt others body)
                    ++ (calcForcesForOne dt model body)
        in
            { body | force = List.foldl add body.force forces }


calcForcesOneVsAll : Time -> List (Node.Model n) -> Node.Model n -> List Vec2
calcForcesOneVsAll dt others body =
    List.filterMap (calcForcesForNoIntersection dt body) others


calcForcesOneOnOne : Time -> List (Node.Model n) -> Node.Model n -> List Vec2
calcForcesOneOnOne dt others body =
    List.map (calcForcesOneOnOneFor dt body) others


calcForcesForOne : Time -> Model n -> Node.Model n -> List Vec2
calcForcesForOne dt model body =
    []



-------------------------------------------------------------


calcForcesForNoIntersection : Time -> Node.Model n -> Node.Model n -> Maybe Vec2
calcForcesForNoIntersection dt elemA elemB =
    let
        minDistance =
            50

        diff =
            (Vec2.sub elemA.pos elemB.pos)

        dist =
            (Vec2.length diff)
    in
        if (dist > minDistance) then
            Nothing
        else
            let
                norm =
                    if (dist < 0.0001) then
                        (vec2 1 1)
                    else
                        (Vec2.normalize diff)

                factor =
                    0.01 + 0.1 * (minDistance - dist)
            in
                Just <| Vec2.scale factor norm


calcForcesOneOnOneFor : Time -> Node.Model n -> Node.Model n -> Vec2
calcForcesOneOnOneFor dt nodeA nodeB =
    let
        -- a und b sollten gleiches x haben
        -- b soll 80 unter a sein
        target =
            (vec2 0 80)

        diff =
            sub nodeB.pos nodeA.pos
    in
        sub diff target
