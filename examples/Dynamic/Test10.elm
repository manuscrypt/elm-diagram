module Dynamic.Test10 exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.App as App
import VirtualDom
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Http
import Task
import Model.ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (fromFiles)
import Model.Layout as Layout
import Dynamic.SvgVisualization
import Time exposing (Time, second)


type alias Model =
    { graph : Graph ElmFile ()
    , labelFn : Node ElmFile -> String
    , error : String
    , layout : Layout.Model
    }


type Msg
    = DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | Tick Time


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update =
            update
            --, subscriptions = ( Time.every (17 * Time.millisecond) Tick )
            -- ] --(\_ -> Sub.none)
        , subscriptions = (\model -> Sub.batch [ Time.every (17 * Time.millisecond) Tick ])
        }


labelFn : { b | label : { a | moduleName : String, name : String } } -> String
labelFn =
    (\x -> x.label.name ++ " (" ++ x.label.moduleName ++ ")")


init : ( Model, Cmd Msg )
init =
    let
        g =
            Graph.empty
    in
        { graph = g
        , labelFn = labelFn
        , error = "no error"
        , layout = Layout.init g labelFn
        }
            ! [ fetchData ]


fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched
        <| Http.get decodeList "http://localhost:3001"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            { model | layout = Layout.animate model.layout } ! []

        ErrorOccurred err ->
            { model | error = toString err } ! []

        DataFetched elmFiles ->
            let
                newGraph =
                    ElmFileGraph.fromFiles elmFiles
            in
                { model
                    | graph = newGraph
                    , layout = Layout.init newGraph (\x -> x.moduleName)
                }
                    ! []



{-
   loadDiagram : Graph ElmFile () -> DynamicDiagram.Model -> DynamicDiagram.Model
   loadDiagram graph diagram =
       let rootNodes = List.filter ( \n -> n.label.moduleName == "Basic" ) <|  Graph.nodes graph
       in List.foldl ( \n d -> loadNode graph n d ) diagram rootNodes
-}


render : Model -> List (VirtualDom.Node a)
render model =
    List.concat
        <| (List.map
                (\n ->
                    Dynamic.SvgVisualization.node (Layout.position n.id model.layout)
                        n.label.name
                )
                (Graph.nodes model.graph)
           )
        ++ (List.map
                (\e ->
                    Dynamic.SvgVisualization.connection (Layout.position e.from model.layout)
                        (Layout.position e.to model.layout)
                )
                (Graph.edges model.graph)
           )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text <| "Error " ++ toString model.error ]
        , Html.div []
            [ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , SA.width "900"
                , SA.height "900"
                , viewBox "0 0 500 500"
                ]
                (render model)
            ]
        ]
