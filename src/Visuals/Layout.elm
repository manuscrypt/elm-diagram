module Visuals.Layout exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add)
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Graph exposing (Graph, Node, Edge, NodeId)
import List.Extra exposing (andThen)
import Time exposing (Time)
import Extra.Vec2Dict as Vec2Dict exposing( Vec2Dict )

type alias Model =
  { graph : Graph Symbol.Model Connection.Model
  , forces: List (NodeId, Vec2)
  }

type Msg
  = Animate Time

init: Graph Symbol.Model Connection.Model -> (Model, Cmd Msg)
init graph =
  { graph = graph
  , forces = List.map (\node -> (node.id, vec2 0 0 ) ) <| Graph.nodes graph
  } ! []

symbols : Model -> List Symbol.Model
symbols model = List.map .label <| Graph.nodes model.graph

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Animate dt ->
      ( animate dt model )
      -- model
      ! []

animate: Time -> Model -> Model
animate dt model =
  let
      forces = calcForces model
      graph' = Graph.mapNodes ( \node ->
          let
            force = ( Vec2Dict.get node.id forces )
            velocity = -- v = a * t + v0
              Vec2.scale ( dt * 0.0001 ) force
              -- + oldVelocity * ( 1.0 - relaxingFactor * dt ) }
            movement = -- s = 0.5 * a * t^2 + v0 * t
              Vec2.scale ( dt ) velocity -- not correct
            newpos = -- p = p0 + s
              Vec2.add node.pos movement
          in
            { node | pos = newpos }
        ) model.graph
  in { model | graph = graph' }

calcForces : Model -> Vec2Dict
calcForces model =
  calcForcesOneVsAll model
  --<| calcForcesOneOnOne model
  --<| calcForcesForOne model
  Vec2Dict.empty

calcForcesOneVsAll : Model -> Vec2Dict -> Vec2Dict
calcForcesOneVsAll model forces =
  let nodes = symbols model
      all = List.filter (\(a,b) -> a /= b)
              <| nodes `andThen` \x ->
                 nodes `andThen` \y ->
                 [(x,y)]
  in List.foldl ( \(a,b) f -> calcForcesForNoIntersection a b f ) forces all

calcForcesForNoIntersection : Symbol.Model -> Symbol.Model -> Vec2Dict -> Vec2Dict
calcForcesForNoIntersection elemA elemB forces =
    let
        minDistance = 40
        posA = elemA.pos
        posB = elemB.pos
        diff = (Vec2.sub posA posB)
        dist = (Vec2.length diff)
    in
        if (dist > minDistance) then
            forces
        else
            let
                norm =
                    if (dist < 0.0001) then
                        (vec2 1 1)
                    else
                        (Vec2.normalize diff)
                factor = 0.01 + 0.1 * (minDistance - dist)
            in
              Vec2Dict.add2
                ( elemA.id, (Vec2.scale factor norm) )
                ( elemB.id, (Vec2.scale -factor norm) )
                forces



applyForce: Float -> (NodeId, Vec2) -> Symbol.Model -> (Symbol.Model, Cmd Symbol.Msg)
applyForce dt (id,force) symbol =
  let pos = Vec2.add symbol.pos (vec2 (0.001 * dt * getX force) (0.001 * dt * getY force))
  in Symbol.update (Symbol.Move pos) symbol

forcesOneVsAll: Model -> Model
forcesOneVsAll model =
  let nodes = symbols model
      all = List.filter (\(a,b) -> a /= b)
              <| nodes `andThen` \x ->
                 nodes `andThen` \y ->
                 [(x,y)]
      iforces = List.concat <| List.map (\(a,b) -> noIntersection a b ) all

  in { model | forces = iforces ++ model.forces }

noIntersection : Symbol.Model -> Symbol.Model -> List ( NodeId, Vec2 )
noIntersection  elemA elemB =
    let
        minDistance = 40
        posA = elemA.pos
        posB = elemB.pos
        diff =
            (Vec2.sub posA posB)

        dist =
            (Vec2.length diff)
    in
        if (dist > minDistance) then
            []
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
                [ ( elemA.id, (Vec2.scale factor norm) )
                , ( elemB.id, (Vec2.scale -factor norm) )
                ]
