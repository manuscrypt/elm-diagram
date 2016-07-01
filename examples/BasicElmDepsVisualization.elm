module BasicElmDepsVisualization exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Diagram
import Layout exposing (LayoutNode, LayoutCell)
import BasicElmDeps
import Graph exposing (Graph, Node, Edge)
import GraphListView
import GraphToDiagram

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
        ( gv, gvx )   = GraphListView.init graph labelFunc
        ( dg, layout) = GraphToDiagram.convert (graph, labelFunc)  

    in
        { layout = layout
        , diagram = dg
        , graphView = gv
        } ! [ Cmd.map GraphViewMsg gvx ]


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
        [ Html.div [ HA.style [(,) "display" "flex", (,) "flex-direction" "column"]]
            [ Html.div[ HA.style [ (,) "flex" "auto" ] ] [ diagram model ]
            , Html.div[ HA.style [ (,) "flex" "auto" ] ] [ App.map GraphViewMsg <| GraphListView.view model.graphView ]
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
