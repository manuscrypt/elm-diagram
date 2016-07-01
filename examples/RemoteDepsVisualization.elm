module RemoteDepsVisualization exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import ElmFileGraph exposing (fromFiles)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Http 
import Task 
import ElmFile exposing (ElmFile, decodeList) 
import GraphListView 
import Layout exposing (LayoutNode, LayoutCell)
import Diagram
import String
import GraphToDiagram
import Extra.Graph
import IntDict

type alias Model 
    = { layout : Layout.Model ElmFile ()
      , graph : Graph ElmFile ()
      , graphView: GraphListView.Model ElmFile () 
      , diagram : Diagram.Model
      , error: String
      }

type Msg 
    = DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | DiagramMsg Diagram.Msg
    | GraphViewMsg GraphListView.Msg

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

init: (Model, Cmd Msg)
init = 
    let (m, mx) = fromGraph Graph.empty
    in m ! [fetchData, mx]  

fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched <|
        Http.get decodeList "http://localhost:3001"

fromGraph: Graph ElmFile () -> (Model, Cmd Msg)
fromGraph graph = 
    let labelFn = (\x -> x.label.name)
        (newView, newViewFx) = GraphListView.init graph labelFn  
        (dg, layout) = GraphToDiagram.convert (graph, labelFn)
    in { graph = graph
       , graphView = newView
       , layout = layout
       , diagram = dg
       , error = "initialized" 
       } ! [Cmd.map GraphViewMsg newViewFx]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ErrorOccurred err -> 
            { model | error = toString err } ! []

        DataFetched elmFiles ->
            let newGraph = fromFiles <| List.filter (\e -> String.length e.moduleName > 0 ) elmFiles
            in
                let basicId = idFor "Basic.elm" elmFiles
                    basicCtx = Graph.get basicId newGraph
                    (bg, bgx) = case basicCtx of 
                        Nothing ->
                            fromGraph <| Graph.inducedSubgraph [basicId ] newGraph
                        Just ctx ->
                            fromGraph <| Graph.inducedSubgraph ([basicId]++(IntDict.keys ctx.outgoing)) newGraph
                in 
                    if not(Extra.Graph.isAcyclic bg.graph) then
                        Debug.crash "not acyclic"
                    else  
                        (bg, bgx)

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

idFor: String -> List ElmFile -> NodeId
idFor str allFiles 
    = List.filter (\e -> e.name == str ) allFiles |> List.head |> Maybe.map (\e -> e.id) |> Maybe.withDefault -1 

view: Model -> Html Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div [] [ Html.text <| "Error " ++ toString model.error ]
        , Html.div [ HA.style [(,) "display" "flex", (,) "flex-direction" "column"]]
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
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]