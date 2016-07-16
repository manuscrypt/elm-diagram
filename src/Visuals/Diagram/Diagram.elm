module Visuals.Diagram.Diagram exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, add, scale, sub)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time exposing (Time)
import Graph exposing (Graph, NodeContext, Node, Edge)
import Visuals.Diagram.Layout as Layout
import Visuals.Diagram.Node as Node
import Visuals.Diagram.Connection as Connection
import Visuals.Diagram.Grid as Grid exposing (GridDef, defaultGridDef)
import Model.CompilationUnit as CompilationUnit
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Model.ElmFile as ElmFile exposing (ElmFile)
import Visuals.Diagram.Node as Node
import Tuple2


type alias Model =
    { size : Vec2
    , grid : Grid.Model
    , nodes : List (Node.Model CompilationUnit.Model)
    , edges : List ( Int, Int )
    }


type Msg
    = Resize Vec2
    | Animate Time


init : ElmFileGraph -> Model
init graph =
    { size = vec2 0 0
    , grid = Grid.empty
    , nodes =
        Graph.nodes graph
            |> List.map .label
            |> List.map fileToNode
    , edges =
        Graph.edges graph
            |> List.map (\e -> ( e.from, e.to ))
    }


fileToNode : ElmFile -> Node.Model CompilationUnit.Model
fileToNode file =
    Node.init file.id (CompilationUnit.init file) (\n -> n.file.moduleName) (initialPosition file.id)


initialPosition : Int -> Vec2
initialPosition index =
    vec2 (150.0 + ((toFloat (index - 20)) * -30.0)) (150.0 + ((toFloat (index - 20)) * 25.0))


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize size ->
            { model
                | size = size
                , grid = Grid.update (Grid.Resize size) model.grid
            }

        Animate dt ->
            { model | nodes = Layout.animate dt model.nodes }


view : Model -> Svg a
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        Svg.svg
            [ SA.version "1.1"
            , SA.width sw
            , SA.height sh
            , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
            , SA.textRendering "optimizeLegibility"
            ]
            ([ Grid.view model.grid ]
                ++ (List.map Node.view model.nodes)
                ++ viewConnections model
            )


viewConnections : Model -> List (Svg a)
viewConnections model =
    List.map (edgeToNodeList model) model.edges
        |> List.map Connection.init
        |> List.map Connection.view


edgeToNodeList : Model -> ( Int, Int ) -> List (Node.Model CompilationUnit.Model)
edgeToNodeList model edge =
    Tuple2.mapBoth (byId model.nodes) edge
        |> Tuple2.toList
        |> List.filterMap identity


byId : List (Node.Model CompilationUnit.Model) -> Int -> Maybe (Node.Model CompilationUnit.Model)
byId nodes id =
    List.head <| List.filter (\n -> n.id == id) nodes
