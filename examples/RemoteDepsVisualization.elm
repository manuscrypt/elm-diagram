module RemoteDepsVisualization exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import ElmFileGraph exposing (fromFiles)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Http
import Task
import IntDict
import ElmFile exposing (ElmFile, decodeList)
import GraphListView
import Layout exposing (LayoutNode, LayoutCell)
import Diagram
import GraphToDiagram
import Window
import Task.Extra as Task


type alias Model =
    { layout : Layout.Model ElmFile ()
    , graph : Graph ElmFile ()
    , graphView : GraphListView.Model ElmFile ()
    , diagram : Diagram.Model
    , message : String
    , size : Window.Size
    }


type Msg
    = DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | DiagramMsg Diagram.Msg
    | GraphViewMsg GraphListView.Msg
    | SetWindowSize Window.Size


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.batch [ Window.resizes SetWindowSize ])
        }


init : ( Model, Cmd Msg )
init =
    let
        ( m, mx ) =
            fromGraph Graph.empty { width = 0, height = 0 }
    in
        m ! [ mx, Window.size |> Task.performFailproof (\s -> SetWindowSize s), fetchData ]


fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched
        <| Http.get decodeList "http://localhost:3001"


fromGraph : Graph ElmFile () -> Window.Size -> ( Model, Cmd Msg )
fromGraph graph size =
    let
        labelFn =
            (\x -> x.label.name)

        ( newView, newViewFx ) =
            GraphListView.init graph labelFn

        ( dg, layout ) =
            GraphToDiagram.convert ( graph, labelFn ) size
    in
        { graph = graph
        , graphView = newView
        , layout = layout
        , diagram = dg
        , message = "initialized: " ++ (toString <| Graph.size graph)
        , size = size
        }
            ! [ Cmd.map GraphViewMsg newViewFx ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize size ->
            { model | size = size } ! []

        ErrorOccurred err ->
            { model | message = toString err } ! []

        DataFetched elmFiles ->
            --fromGraph (fromFiles elmFiles) model.size
            let
                newGraph =
                    fromFiles elmFiles

                --<| List.filter (\e -> String.length e.moduleName > 0 ) elmFiles
            in
                let
                    basicId =
                        idFor "Basic.elm" elmFiles

                    basicCtx =
                        Graph.get basicId newGraph

                    subGraph =
                        case basicCtx of
                            Nothing ->
                                (Graph.inducedSubgraph [ basicId ] newGraph)

                            Just ctx ->
                                (Graph.inducedSubgraph ([ basicId ] ++ (IntDict.keys ctx.incoming)) newGraph)
                in
                    fromGraph subGraph model.size

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


idFor : String -> List ElmFile -> NodeId
idFor str allFiles =
    List.filter (\e -> e.name == str) allFiles |> List.head |> Maybe.map (\e -> e.id) |> Maybe.withDefault -1


view : Model -> Html Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div [] [ Html.text <| "Message: " ++ toString model.message ]
        , Html.div [ HA.style [ (,) "display" "flex", (,) "flex-direction" "column" ] ]
            [ Html.div [ HA.style [ (,) "flex" "auto" ] ] [ diagram model ]
            , Html.div [ HA.style [ (,) "flex" "auto" ] ] [ App.map GraphViewMsg <| GraphListView.view model.graphView ]
            , Html.div [ HA.style [ (,) "flex" "auto" ] ] [ Html.text <| Graph.toString' model.graph ]
            ]
        , Html.div
            [ HA.style
                [ (,) "clear" "both"
                , (,) "position" "relative"
                ]
            ]
            [ Html.text <| toString model ]
        ]


diagram : Model -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ -- (,) "width" "920px"
          -- , (,) "margin" "auto"
          -- , (,) "border" "1px solid black"
          (,) "background-color" "#EEEEEE"
        ]
