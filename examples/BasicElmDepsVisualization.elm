module BasicElmDepsVisualization exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Diagram
import Symbol
import Connection
import Layout exposing (LayoutNode, LayoutCell)
import BasicElmDeps
import Graph exposing (Graph, Node, Edge)
import GraphListView


type alias Model a =
    { layout : Layout.Model a ()
    , diagram : Diagram.Model
    , graphView : GraphListView.Model a ()
    }


type Msg
    = DiagramMsg Diagram.Msg
    | GraphViewMsg GraphListView.Msg


init : ( Graph a (), Node a -> String ) -> ( Model a, Cmd Msg )
init ( graph, labelFunc ) =
    let
        ( gv, gvx ) =
            GraphListView.init graph labelFunc

        layout =
            Layout.init graph labelFunc

        nodes =
            Layout.getNodes layout

        ( syms, symsFx ) =
            createSymbols nodes

        conns =
            createConnections syms layout graph

        ( dg, dgFx ) =
            Diagram.init syms conns
    in
        { layout = layout
        , diagram = dg
        , graphView = gv
        }
            ! [ Cmd.map DiagramMsg dgFx
              , Cmd.map GraphViewMsg gvx
              ]


nodeToSymbol : Int -> Layout.LayoutNode -> ( Symbol.Model, Cmd Symbol.Msg )
nodeToSymbol index node =
    Symbol.init index node.label node.color (vec2 20 20) node.pos


createSymbols : List Layout.LayoutNode -> ( List Symbol.Model, List (Cmd Symbol.Msg) )
createSymbols nodes =
    List.unzip <| List.indexedMap nodeToSymbol nodes


createConnections : List Symbol.Model -> Layout.Model a () -> Graph a () -> List Connection.Model
createConnections syms layout graph =
    let
        edges =
            Graph.edges graph
    in
        List.foldl (createConnection syms layout graph) [] edges


createConnection : List Symbol.Model -> Layout.Model a () -> Graph a () -> Edge () -> List Connection.Model -> List Connection.Model
createConnection symbols layout graph edge conns =
    let
        cells =
            edgeToCells edge layout graph
    in
        conns ++ [ Connection.init <| cellsToSymbols cells symbols ]


edgeToCells : Edge () -> Layout.Model a () -> Graph a () -> List (Layout.LayoutCell a)
edgeToCells e layout graph =
    let
        vs =
            List.map .node <| List.filterMap (\id -> Graph.get id graph) [ e.from, e.to ]
    in
        List.filterMap (Layout.getCell layout) vs


cellsToSymbols : List (Layout.LayoutCell a) -> List Symbol.Model -> List Symbol.Model
cellsToSymbols cells symbols =
    let
        idxs =
            List.map .index cells
    in
        List.filter (\s -> List.member s.id idxs) symbols


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )

        GraphViewMsg msg ->
            let
                ( gv, gvx ) =
                    GraphListView.update msg model.graphView
            in
                { model | graphView = gv } ! [ Cmd.map GraphViewMsg gvx ]


diagram : Model a -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


view : Model a -> Svg Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div []
            [ diagram model
            , App.map GraphViewMsg <| GraphListView.view model.graphView
            ]
        , Html.div
            [ HA.style
                [ (,) "clear" "both"
                , (,) "position" "relative"
                ]
            ]
            [ Html.text <| toString model ]
        ]


main : Program Never
main =
    App.program
        { init = init BasicElmDeps.sample2
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]
