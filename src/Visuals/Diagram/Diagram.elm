module Visuals.Diagram.Diagram exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub)
import Svg exposing (Svg)
import Html.App as App
import Svg.Attributes as SA
import Time exposing (Time)
import Graph exposing (Graph, NodeContext, Node, Edge)
--import Visuals.Diagram.Layout as Layout
import Visuals.Diagram.Connection as Connection
import Visuals.Diagram.Grid as Grid exposing (GridDef, defaultGridDef)
import Model.CompilationUnit as CompilationUnit
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Visuals.Layout.Force.Body as Body
import Tuple2
import Visuals.Layout.Force.ForceLayoutCalculator as Layout
import Visuals.Layout.ViewportCalculator as ViewportCalculator
import String

type alias Model =
    { size : Vec2
    , grid : Grid.Model
    , graph : Graph CompilationUnit.Model ()
    , layout: Layout.Model
    }

type Msg
    = NoOp
    | Resize Vec2
    | Animate Time

init : ElmFileGraph -> Model
init graph =
    { size = vec2 0 0
    , grid = Grid.empty
    , graph = CompilationUnit.fromElmFileGraph graph
    , layout = Layout.init graph
    }

viewBox : Model -> String
viewBox model =
  let span = model.layout.state.span
      (minx, miny) = (getX span.min, getY span.min)
      (maxx, maxy) = (getX span.max, getY span.max)
  in  String.join " " <| List.map toString [ minx, miny, (maxx - minx), (maxy - miny) ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Resize size ->
            { model
                | size = size
                , grid = Grid.update (Grid.Resize size) model.grid
            }

        Animate dt ->
          let layout = model.layout
              layout' =
                  { layout |  state = layout.step Layout.nodeView dt layout.state }

          in { model | layout = layout' }

calcViewport : Model -> String
calcViewport model =
  let positions = List.map ( \n -> n.label.pos ) <| Graph.nodes model.layout.state.graph
  in ViewportCalculator.fromPositions 100 positions

view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        Svg.svg
            [ SA.version "1.1"
            , SA.width sw
            , SA.height sh
            --, SA.viewBox <| "0 0 800 600" -- <| Layout.viewBox model.nodes
            , SA.viewBox ( calcViewport model )
            , SA.textRendering "optimizeLegibility"
            ]
            ([ Grid.view model.grid ]
                ++ viewNodes model
                ++ viewConnections model
            )


viewNodes: Model  -> List (Svg Msg)
viewNodes model =
  List.map (\n ->
      let (vec,view) = model.layout.at Layout.nodeView model.layout.state n.id
      in App.map (\_ -> NoOp ) <| view ) <| Graph.nodes model.layout.state.graph



viewConnections : Model -> List (Svg a)
viewConnections model =
  Graph.edges model.layout.state.graph |>
      List.map (edgeToNodeList model)
        |> List.map Connection.init
        |> List.map Connection.view


edgeToNodeList : Model -> Edge (List Vec2) -> List Body.Model
edgeToNodeList model {from, to, label} =
    Tuple2.mapBoth (byId model.layout.state.graph) (from,to)
        |> Tuple2.toList
        |> List.filterMap identity


byId : Graph Body.Model (List Vec2) -> Int -> Maybe Body.Model
byId graph id =
  Maybe.map (.label << .node) <| Graph.get id graph
