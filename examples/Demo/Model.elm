module Demo.Model exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Http
import Extra.Http as Http
import Graph exposing (Graph, Node)
import Time exposing (Time)
import AnimationFrame
import Window exposing (Size)
import Task
import Task.Extra as Task exposing (performFailproof)
import Random exposing (Seed, initialSeed)
import Date exposing (Date)
import Model.ElmFile as ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Json.Decode as Json
import Json.Encode as Json
import Result
import Visuals.GraphListView as GraphListView
import Visuals.Visualization as Visualization
import Maybe
import IntDict


type Msg
    = Init Time
    | ServerReachable Bool
    | SetErrorMsg String
    | Resize Size
    | TextInputChanged String
    | Start
    | Animate Time
    | Stop
    | FetchRemote
    | SetGraph (ElmFileGraph)
    | RemoteFetched (List ElmFile)
    | VisualizationMsg Visualization.Msg
    | GraphListViewMsg GraphListView.Msg


type alias Model =
    { graph : ElmFileGraph
    , graphString : String
    , lastDt : Time
    , size : Vec2
    , errorMsg : String
    , now : Maybe Date
    , seed : Seed
    , running : Bool
    , serverRunning : Bool
    , visualization : Visualization.Model
    , listView : GraphListView.Model ElmFile ()
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.running then
            AnimationFrame.diffs Animate
          else
            Sub.none
        , Window.resizes Resize
        ]


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.empty

        ( lv, lvCmd ) =
            GraphListView.init graph (\node -> node.label.moduleName)

        ( vis, visCmd ) =
            Visualization.init graph (vec2 0 0)
    in
        { graph = graph
        , graphString = ""
        , lastDt = 0
        , size = vec2 0 0
        , errorMsg = ""
        , seed = initialSeed Random.minInt
        , now = Nothing
        , running = False
        , serverRunning = False
        , visualization = vis
        , listView = lv
        }
            ! [ Time.now |> performFailproof Init
              , checkServerStatus
              , Cmd.map GraphListViewMsg lvCmd
              , Cmd.map VisualizationMsg visCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init time ->
            { model
                | now = Just <| Date.fromTime time
                , seed = Random.initialSeed (round time)
            }
                ! [ Window.size |> performFailproof Resize ]

        ServerReachable b ->
            { model
                | serverRunning = b
                , errorMsg =
                    if b then
                        "Server available"
                    else
                        "Server unreachable"
            }
                ! []

        SetGraph graph ->
            let
                ( lv, lvCmd ) =
                    GraphListView.init graph (\x -> x.label.moduleName)

                ( vis, visCmd ) =
                    Visualization.init graph model.size
            in
                { model
                    | graph = graph
                    , graphString = Json.encode 2 <| ElmFile.encodeList <| List.map .label <| Graph.nodes graph
                    , listView = lv
                    , visualization = vis
                    , errorMsg = "Graph set"
                }
                    ! [ Cmd.map GraphListViewMsg lvCmd
                      , Cmd.map VisualizationMsg visCmd
                      ]

        GraphListViewMsg msg ->
            let
                ( gv, gvx ) =
                    GraphListView.update msg model.listView
            in
                case msg of
                    GraphListView.SelectNode nodeId ->
                        let
                            nodeIds =
                                Maybe.withDefault [] <| Maybe.map IntDict.keys <| Maybe.map .outgoing <| Graph.get nodeId model.graph
                        in
                            update (SetGraph <| Graph.inducedSubgraph nodeIds model.graph) model

                    _ ->
                        { model | listView = gv } ! [ Cmd.map GraphListViewMsg gvx ]

        TextInputChanged str ->
            let
                graph =
                    Result.withDefault Graph.empty
                        <| Result.map ElmFileGraph.fromFiles
                        <| Json.decodeString ElmFile.decodeList str
            in
                update (SetGraph graph) model

        Start ->
            { model | running = True } ! []

        Stop ->
            { model | running = False } ! []

        SetErrorMsg strErr ->
            { model | errorMsg = strErr } ! []

        Animate dt ->
            let
                ( anim, animCmd ) =
                    Visualization.update Visualization.Animate model.visualization
            in
                { model | lastDt = dt, visualization = anim } ! [ Cmd.map VisualizationMsg animCmd ]

        Resize size ->
            { model | size = vec2 (toFloat size.width) (toFloat size.height) } ! []

        FetchRemote ->
            { model | errorMsg = "Fetching..." } ! [ fetchData RemoteFetched ]

        RemoteFetched elmFiles ->
            update (SetGraph <| ElmFileGraph.fromFiles elmFiles) model

        VisualizationMsg msg ->
            let
                ( d, dx ) =
                    Visualization.update msg model.visualization
            in
                { model | visualization = d } ! [ Cmd.map VisualizationMsg dx ]


fetchData : (List ElmFile -> Msg) -> Cmd Msg
fetchData msg =
    Task.perform (\err -> SetErrorMsg <| Http.httpErrorToString err) msg
        <| Http.get decodeList "http://localhost:3001"


checkServerStatus : Cmd Msg
checkServerStatus =
    Task.perform (\_ -> ServerReachable False) (\_ -> ServerReachable True)
        <| head "http://localhost:3001"


head : String -> Task.Task Http.RawError Http.Response
head url =
    Http.send Http.defaultSettings
        { verb = "HEAD"
        , headers = []
        , url = url
        , body = Http.empty
        }
