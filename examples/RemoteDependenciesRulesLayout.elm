module RemoteDependenciesRulesLayout exposing (..)

import Window
import Html exposing (Html)
import Html.App as App
import Time exposing (Time, second)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Task
import Http
import IntDict
import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Model.ElmFile as ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (fromFiles)
import Dynamic.Diagram as DynamicDiagram
import Layouts.Rules as DynamicLayout


type alias Model =
    { message : String
    , graph : Graph ElmFile ()
    , files : List ElmFile
    , diagram : DynamicDiagram.Model
    }


type Msg
    = Tick Time
    | DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | SetWindowSize Window.Size


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.batch [ Window.resizes SetWindowSize, Time.every Time.millisecond Tick ])
        }


init : ( Model, Cmd Msg )
init =
    { message = "Hello"
    , files = []
    , graph = Graph.empty
    , diagram = DynamicDiagram.init
    }
        ! [ (Task.perform ErrorOccurred DataFetched <| Http.get decodeList "http://localhost:3001/")
          ]


addImport : Node ElmFile -> Node ElmFile -> Model -> Model
addImport parentNode importNode model =
    { model
        | diagram =
            DynamicDiagram.addImport { name = parentNode.label.name }
                { name = importNode.label.name }
                model.diagram
    }



--        model


loadRoot : Node ElmFile -> Model -> Model
loadRoot root model =
    case Graph.get root.id model.graph of
        Nothing ->
            Debug.crash "ctx not found"

        Just ctx ->
            let
                induced =
                    Debug.log "induces"
                        <| Graph.inducedSubgraph ((IntDict.keys ctx.outgoing)) model.graph
            in
                List.foldl
                    (\node m ->
                        (addImport root
                            node
                            (if (DynamicDiagram.containsNode { name = node.label.name } model.diagram) then
                                m
                             else
                                { m | diagram = DynamicDiagram.addNode { name = node.label.name } m.diagram }
                            )
                        )
                    )
                    { model | diagram = DynamicDiagram.addRootNode { name = root.label.name } model.diagram }
                    (Graph.nodes induced)


loadElmFiles : List ElmFile -> Model -> Model
loadElmFiles files model =
    let
        graph =
            fromFiles files
    in
        List.foldl (\root m -> (loadRoot root m)) { model | graph = graph }
            <| List.filter (\n -> n.label.name == "BasicDiagram.elm")
            <| Graph.nodes graph


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize size ->
            { model
                | message = "SetWindowSize " ++ (toString size)
            }
                ! []

        Tick newTime ->
            { model
                | diagram = DynamicDiagram.animate model.diagram
                , message = toString newTime
            }
                ! []

        ErrorOccurred err ->
            { model | message = toString err } ! []

        DataFetched elmFiles ->
            (loadElmFiles elmFiles model) ! []


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text model.message ]
        , Html.div []
            [ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , SA.width "1600"
                , SA.height "1500"
                , viewBox (DynamicLayout.viewboxAsString 100 model.diagram.layout)
                ]
                (DynamicDiagram.view model.diagram)
            ]
        ]
