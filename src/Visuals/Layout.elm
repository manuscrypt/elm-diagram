module Visuals.Layout exposing (..)

import Math.Vector2 as Vec2 exposing (vec2, getX, getY, add)
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Graph exposing (Graph, Node, Edge, NodeId)

import Time exposing (Time)

type alias Model =
  { graph : Graph Symbol.Model Connection.Model
  }

type Msg
  = Animate Time

init: Graph Symbol.Model Connection.Model -> (Model, Cmd Msg)
init graph =
  { graph = graph } ! []

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Animate dt ->
      { model | graph = Graph.mapNodes (animate dt) model.graph } ! []

animate: Time -> Symbol.Model -> Symbol.Model
animate dt symbol =
  let pos = Vec2.add symbol.pos (vec2 (dt * 0.1) (dt * 0.1))
      (sym, cmd) = Symbol.update (Symbol.Move pos) symbol
  in sym
