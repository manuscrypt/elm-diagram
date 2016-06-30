module Dependencies exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.App as App
import Http 
import Task 
import String 
import ElmFile exposing (ElmFile, decodeList) 
import GraphListView 

type alias Model 
    = { graph : Graph ElmFile ()
      , graphView: GraphListView.Model ElmFile () 
      , error: String
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

init: (Model, Cmd Msg)
init = 
    let g = Graph.empty
        (gv, gvx) = GraphListView.init g (\x -> x.label.name)
    in { graph = g
       , graphView = gv  
       , error = "no error" 
       } ! [ fetchData, Cmd.map GraphListViewMsg gvx ] 

fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched <|
        Http.get decodeList "http://localhost:3001"

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        ErrorOccurred err -> 
            { model | error = toString err } ! []

        DataFetched elmFiles ->
             let newGraph = fromFiles elmFiles
                 (newView, newViewFx) = GraphListView.init newGraph (\x -> x.label.name) 
            in { model | graph = newGraph, graphView = newView } 
                    ! [Cmd.map GraphListViewMsg newViewFx]

        GraphListViewMsg msg ->
            let (gv, gvx) = GraphListView.update msg model.graphView
            in { model | graphView = gv } ! [Cmd.map GraphListViewMsg gvx]

fromFiles: List ElmFile -> Graph ElmFile ()
fromFiles files =
    let nodes = List.map (\o -> Node o.id o) files
        edges = makeEdges files 
    in Graph.fromNodesAndEdges nodes edges

makeEdges: List ElmFile -> List (Edge ())
makeEdges files =
    List.foldl (makeEdge files) [] files

makeEdge: List ElmFile -> ElmFile -> List (Edge ())-> List (Edge ())
makeEdge all file edges = 
    let imps = List.filterMap (findFile all) file.imports
    in edges ++ (List.map (\f -> Edge file.id f.id ()) imps) 

findFile: List ElmFile -> String -> Maybe ElmFile
findFile files name =
    Maybe.oneOf 
        [ List.head <| List.filter (\f -> f.moduleName == name ) files
        , List.head <| List.filter (\f -> (String.dropRight 4 f.name) == name ) files
        ]

view: Model -> Html Msg
view model =
    Html.div [] [ Html.div [] [Html.text <| "Error " ++ toString model.error ]
                , App.map GraphListViewMsg <| GraphListView.view model.graphView  
                ]