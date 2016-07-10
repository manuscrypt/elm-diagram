module ProjectDependencies exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.App as App
import Http
import Task
import Model.ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (fromFiles)
import Visuals.GraphListView as GraphListView


type alias Model =
    { graph : Graph ElmFile ()
    , graphView : GraphListView.Model ElmFile ()
    , labelFn : Node ElmFile -> String
    , error : String
    }


type Msg
    = DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | GraphListViewMsg GraphListView.Msg


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : ( Model, Cmd Msg )
init =
    let
        g =
            Graph.empty

        labelFn =
            (\x -> x.label.name ++ " (" ++ x.label.moduleName ++ ")")

        ( gv, gvx ) =
            GraphListView.init g labelFn
    in
        { graph = g
        , graphView = gv
        , labelFn = labelFn
        , error = "no error"
        }
            ! [ fetchData, Cmd.map GraphListViewMsg gvx ]


fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched
        <| Http.get decodeList "http://localhost:3001"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErrorOccurred err ->
            { model | error = toString err } ! []

        DataFetched elmFiles ->
            let
                newGraph =
                    ElmFileGraph.fromFiles elmFiles

                ( newView, newViewFx ) =
                    GraphListView.init newGraph model.labelFn
            in
                { model | graph = newGraph, graphView = newView }
                    ! [ Cmd.map GraphListViewMsg newViewFx ]

        GraphListViewMsg msg ->
            let
                ( gv, gvx ) =
                    GraphListView.update msg model.graphView
            in
                { model | graphView = gv } ! [ Cmd.map GraphListViewMsg gvx ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text <| "Error " ++ toString model.error ]
        , App.map GraphListViewMsg <| GraphListView.view model.graphView
        ]
