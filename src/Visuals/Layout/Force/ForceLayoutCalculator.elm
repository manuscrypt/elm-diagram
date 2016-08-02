module Visuals.Layout.Force.ForceLayoutCalculator exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub, length)
import Time exposing (Time)
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes as SA
import Visuals.Layout.LayoutCalculator exposing (LayoutCalculator, NodeView, Span)
import Graph exposing (Graph, NodeContext, Node, Edge)
import Model.ElmFile as ElmFile exposing (ElmFile)
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Visuals.Layout.Force.Body as Body exposing (origin)
import Visuals.Layout.Force.Model as Model exposing (MyNodeContext, MyState, MyGraph, MyNode)
import IntDict

type Msg = NoOp

type alias Model =
   LayoutCalculator MyState Body.Model (Svg Msg)

fiddle : Time -> Body.Model -> Body.Model
fiddle time body =
  { body | force = scale (0.0001) body.force
               --, vel = scale (0.54) body.vel
               , accel = scale (0.019) body.accel
               }

zeroSpan : Span
zeroSpan = { min = origin, max = origin }

init: ElmFileGraph -> Model
init graph =
  let state =
    { graph = fromElmFileGraph graph
    , span = { min = vec2 0 0, max = vec2 0 0 }
    }
  in { state = state
     , step = step
     , at  = at
     }

nodeView: NodeView Body.Model (Svg Msg)
nodeView {id, label} =
  ( zeroSpan
  , App.map (\_ -> NoOp) <| Body.view label
  )

at: NodeView Body.Model (Svg Msg) -> MyState -> Int -> (Vec2, (Svg Msg))
at viewFunc state id =
  case Graph.get id state.graph of
    Nothing ->
      (origin, Svg.g [][] )
    Just ctx ->
      let (newSpan, view ) = viewFunc ctx.node
      in (ctx.node.label.pos, view)

step: NodeView Body.Model (Svg Msg)-> Time -> MyState -> MyState
step viewFunc dt state =
    let
        timeInSecs =
            dt / 1000
    in
      let all = List.map .label <| Graph.nodes <| state.graph
          forced = calcForces dt all state.graph
          adjusted = Graph.mapNodes (fiddle dt) forced
          moved = Graph.mapNodes (Body.forward dt) adjusted
      in { state | graph = moved }


calcForces: Time -> List Body.Model -> MyGraph -> MyGraph
calcForces dt bodies graph =
  Graph.mapNodes (allVsOne dt bodies) graph
    |> Graph.mapContexts (oneVsOne dt bodies)

allVsOne : Time -> List Body.Model -> Body.Model -> Body.Model
allVsOne dt others body =
  { body | force = List.foldl (calcForcesForNoIntersection dt body) body.force  others }

calcForcesForNoIntersection : Time -> Body.Model -> Body.Model -> Vec2 -> Vec2
calcForcesForNoIntersection dt elemA elemB force =
  if elemA.id == elemB.id then
    force
  else
    let
        minDistance =
            50

        diff =
            (sub elemA.pos elemB.pos)

        dist =
            (length diff)
    in
        if (dist > minDistance) then
            force
        else
            add diff force




oneVsOne : Time -> List Body.Model -> NodeContext Body.Model (List Vec2) -> NodeContext Body.Model (List Vec2)
oneVsOne dt others ({ node, incoming, outgoing } as ctx) =
  let deps = List.filterMap (getNode others) <| IntDict.keys incoming
      body = node.label
      body' = { body | force = List.foldl (sameXAndBelow dt body) body.force deps }
  in { ctx  | node = Node node.id body' }

{-
        -- a und b sollten gleiches x haben
        -- b soll 80 unter a sein
-}
sameXAndBelow : Time -> Body.Model -> Body.Model -> Vec2 -> Vec2
sameXAndBelow dt nodeA nodeB force =
  if nodeA == nodeB then
    force
  else
    let
        x = if nodeA.id % 2 == 0 then 25 else -25
        target =
            (vec2 x -80)

        diff =
            sub nodeB.pos nodeA.pos
    in
        -- needed? add force <| 
          sub diff target



initialPosition : Int -> Vec2
initialPosition index =
    vec2 (150.0 + ((toFloat (index - 20)) * -15.0)) (150.0 + ((toFloat (index - 20)) * 25.0))


sample1 : ElmFileGraph
sample1 =
    ElmFileGraph.fromFiles
        [ ElmFile 21 "21" "/path/21" 21 [ "22", "23", "24" ] "21"
        , ElmFile 22 "22" "/path/22" 22 [ "24" ] "22"
        , ElmFile 23 "23" "/path/23" 23 [ "24" ] "23"
        , ElmFile 24 "24" "/path/24" 24 [] "24"
        ]


getNode: List (Body.Model) -> Int -> Maybe (Body.Model)
getNode list id =
  List.head <| List.filter (\n -> n.id == id ) list


fromElmFileGraph: ElmFileGraph -> Graph Body.Model (List Vec2)
fromElmFileGraph fileGraph =
  Graph.mapEdges (always []) <| Graph.mapNodes convertNode fileGraph

convertNode : ElmFile -> Body.Model
convertNode file =
    Body.init
      file.id
      file.moduleName
      (initialPosition file.id)

main : Svg Msg
main =
  let model = init sample1
      nextState = model.step nodeView 2.54 model.state
      (pos21, svg21 ) = model.at nodeView nextState 21

  in Svg.svg [ SA.version "1.1", SA.x "0", SA.y "0", SA.viewBox "0 0 323.141 322.95" ]
             [svg21]


--
--
--
-- -- let
-- --     norm =
-- --         if (dist < 0.0001) then
-- --             (vec2 1 1)
-- --         else
-- --             (normalize diff)
-- --
-- --     factor =
-- --         0.01 + 0.1 * (minDistance - dist)
-- -- in
-- --     Just <| scale factor norm




-- calcForcesForOne : Time -> MyNodeContext -> MyNodeContext
-- calcForcesForOne dt ctx state =
--   let thisBody = ctx.node.label
--       intersected = List.foldl (calcForcesForNoIntersection dt body) <| Graph.nodes state.graph
--
--       nodes =
--       others = List.filter (\n -> n.id /= ctx.node.id) nodes
--       inc = IntDict.values ctx.incoming
--       out = IntDict.values ctx.outgoing
--       forAll = Graph.mapNodes (calcForcesOneVsAll dt ctx others) state.graph
--   in { state | graph = forAll }
--       --          ++ calcForcesOneOnOne dt ctx
--       --|> ++ calcForcesForOne dt ctx state

--
-- calcForcesOneOnOne : Time -> MyNodeContext -> MyState -> MyState
-- calcForcesOneOnOne dt ctx state =
--     let incForces = List.map (\incNode ->
--                       let fx = ( getX body.pos )
--                           fy = ( getY body.pos )
--                           tx = ( getX incNode.pos )
--                           ty = ( getY incNode.pos )
--                       in
--                         vec2
--                           (
--                             if( fx < tx )then 5 else if( fx > tx ) then -5 else 0
--                           )
--                           (  if( fy <= ty + 50 )then
--                                 15 --( 0.51 * ( fy - 0 - ty ) )
--                              else
--                                 0
--                           )
--                     ) inc
--         --outEdges = List.filter ( \( fromId, toId ) -> toId == body.id )
--     in
--       --List.concat <|
--         --List.map ( \( fromId, toId ) -> calcForcesOneOnOneFor dt body) edges
--         incForces
--
--
-- -- calcForcesForOne : Time -> Model a -> Body.Model -> List Vec2
-- -- calcForcesForOne dt model body =
-- --     [ scale -0.8 body.vel ]
--
--
-- -------------------------------------------------------------
--
--
--
--
