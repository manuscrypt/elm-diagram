module Old.Visualization exposing (..)

import Window
import Html exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Visuals.GraphListView as GraphListView
import Visuals.Diagram as Diagram
import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Old.Layout as Layout exposing (LayoutNode, LayoutCell)
import Old.GraphToDiagram


type alias Model a b =
    { layout : Layout.Model a b
    , graph : Graph a b
    , graphView : GraphListView.Model a b
    , diagram : Diagram.Model
    , message : String
    , size : Window.Size
    }


type Msg
    = DiagramMsg Diagram.Msg
    | GraphViewMsg GraphListView.Msg
    | SetWindowSize Window.Size


init : Graph a b -> Window.Size -> (Node a -> String) -> ( Model a b, Cmd Msg )
init graph size labelFn =
    let
        ( newView, newViewFx ) =
            GraphListView.init graph labelFn

        ( dg, layout ) =
            Old.GraphToDiagram.convert ( graph, labelFn ) size
    in
        { graph = graph
        , graphView = newView
        , layout = layout
        , diagram = dg
        , message = "initialized: " ++ (toString <| Graph.size graph)
        , size = size
        }
            ! [ Cmd.map GraphViewMsg newViewFx ]


update : Msg -> Model a b -> ( Model a b, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize size ->
            { model | size = size } ! []

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


view : Model a b -> Html Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div [] [ Html.text <| "Message: " ++ toString model.message ]
        , Html.div [ HA.style [ (,) "display" "flex", (,) "flex-direction" "column" ] ]
            [ Html.div [ HA.style [ (,) "flex" "auto" ] ] [ diagram model ]
            , Html.div [ HA.style [ (,) "flex" "auto" ] ] [ App.map GraphViewMsg <| GraphListView.view model.graphView ]
            , Html.div [ HA.style [ (,) "flex" "auto" ] ] [ Html.text <| Graph.toString' model.graph ]
            ]
          -- , Html.div
          --     [ HA.style
          --         [ (,) "clear" "both"
          --         , (,) "position" "relative"
          --         ]
          --     ]
          --     [ Html.text <| toString model ]
        ]


diagram : Model a b -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            , (,) "border" "1px solid violet"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ -- (,) "width" "920px"
          -- , (,) "margin" "auto"
          (,) "background-color" "#EEEEEE"
        , (,) "padding" "10px"
        ]
