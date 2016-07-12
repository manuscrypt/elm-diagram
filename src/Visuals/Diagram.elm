module Visuals.Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes as SA
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Visuals.Grid as Grid
import Graph exposing (Graph, Node, NodeId, Edge)
import Model.CompilationUnit as CompilationUnit
import Visuals.Layout as Layout
import IntDict
import AnimationFrame
import Time exposing (Time)

type alias Model =
    { size : Vec2
    , grid : Grid.Model
    , graph : Graph Symbol.Model Connection.Model
    , layout: Layout.Model
    , running: Bool
    }


type Msg
    = NoOp
    | Modify Int Symbol.Msg
    | Animate Time
    | LayoutMsg Layout.Msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.running then
            AnimationFrame.diffs Animate
          else
            Sub.none
        ]


init : Vec2 -> Graph Symbol.Model Connection.Model -> ( Model, Cmd Msg )
init size graph =
    let
        w =
            (getX size)

        h =
            (getY size)

        (l,lx) = Layout.init graph
    in
        { size = vec2 w h
        , grid = Grid.init ( 0, w ) ( 0, h ) 25
        , graph = graph
        , layout = l
        , running = True
        }
            ! [Cmd.map LayoutMsg lx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Modify id msg ->
            case Graph.get id model.graph of
                Nothing ->
                    model ! []

                Just ctx ->
                    let
                        ( sym', eff ) =
                            Symbol.update msg ctx.node.label

                        newCtx =
                            { ctx | node = Node id sym' }
                    in
                        ( { model | graph = Graph.update id (always <| Just newCtx) model.graph }
                        , Cmd.map (Modify id) eff
                        )
        Animate dt ->
          update (LayoutMsg <| Layout.Animate dt) model

        LayoutMsg msg ->
          let (l,lx) = Layout.update msg model.layout
          in { model | layout = l, graph = l.graph } ! [ Cmd.map LayoutMsg lx ]

viewSymbol : Symbol.Model -> Svg Msg
viewSymbol sym =
    App.map (Modify sym.id) (Symbol.view sym)


symbols : Model -> List Symbol.Model
symbols model =
    List.map .label <| Graph.nodes model.graph


connections : Model -> List Connection.Model
connections model =
    List.map .label <| Graph.edges model.graph


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
            , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
            , SA.textRendering "optimizeLegibility"
            ]
            ([ Grid.view model.grid ]
                ++ (List.map viewSymbol <| symbols model)
                ++ (List.map Connection.view <| connections model)
            )



toSymbol : Node CompilationUnit.Model -> Node Symbol.Model
toSymbol node =
   Node node.id (fst <| Symbol.init node.id node.label.file.moduleName ( vec2 0 0 ) )

toConnection : List (Node Symbol.Model) -> Edge () -> Edge Connection.Model
toConnection symbols edge =
    let
        filtered =
            List.map .label
                <| List.filter (\s -> List.member s.id [ edge.from, edge.to ]) symbols
    in
        Edge edge.from edge.to <| Connection.init filtered

diagramGraphFromCompilationUnitGraph : Graph CompilationUnit.Model () -> Graph Symbol.Model Connection.Model
diagramGraphFromCompilationUnitGraph g =
    let
        nodes =
            List.map toSymbol <| Graph.nodes g

        conns =
            List.map (toConnection nodes) <| Graph.edges g
    in
        Graph.fromNodesAndEdges nodes conns
