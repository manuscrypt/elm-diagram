module Visuals.Layout.Force.Model exposing (..)

import Visuals.Layout.LayoutCalculator exposing (LayoutCalculator, NodeView, Span)
import Visuals.Layout.Force.Body as Body
import Graph exposing (Graph, NodeContext, Node, Edge)
import Math.Vector2 as Vec2 exposing (Vec2)

type alias MyNode = Node Body.Model
type alias MyGraph = Graph Body.Model (List Vec2)
type alias MyState = {graph: MyGraph, span: Span}
type alias MyNodeContext = NodeContext Body.Model (List Vec2)
