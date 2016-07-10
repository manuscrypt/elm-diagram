module Demo.Model exposing (..)

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
import Model.DiagramLayout as DiagramLayout
import Model.Layout as Layout
import Layouts.Rules as Rules
import Visuals.GraphListView as GraphListView


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
    | DiagramLayoutMsg DiagramLayout.Msg
    | GraphListViewMsg GraphListView.Msg


type alias Model =
    { graph : ElmFileGraph
    , graphString : String
    , lastDt : Time
    , size : Size
    , errorMsg : String
    , now : Maybe Date
    , seed : Seed
    , running : Bool
    , serverRunning : Bool
    , layout : DiagramLayout.Model
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

        ( lay, layCmd ) =
            DiagramLayout.init <| Layout.init graph

        ( lv, lvCmd ) =
            GraphListView.init graph (\node -> node.label.moduleName)
    in
        { graph = graph
        , graphString = ""
        , lastDt = 0
        , size = { width = 0, height = 0 }
        , errorMsg = ""
        , seed = initialSeed Random.minInt
        , now = Nothing
        , running = False
        , serverRunning = False
        , layout = lay
        , listView = lv
        }
            ! [ Time.now |> performFailproof Init
              , checkServerStatus
              , Cmd.map GraphListViewMsg lvCmd
              , Cmd.map DiagramLayoutMsg layCmd
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
            { model | serverRunning = b } ! []

        SetGraph graph ->
            let
                ( lay, layCmd ) =
                    DiagramLayout.init <| Layout.init graph
            in
                { model
                    | graph = graph
                    , layout = lay
                    , graphString = Json.encode 2 <| ElmFile.encodeList <| List.map .label <| Graph.nodes graph
                    , errorMsg = "Graph set"
                }
                    ! [ Cmd.map DiagramLayoutMsg layCmd ]

        GraphListViewMsg msg ->
            let
                ( gv, gvx ) =
                    GraphListView.update msg model.listView
            in
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
                    DiagramLayout.update DiagramLayout.Animate model.layout
            in
                { model | lastDt = dt, layout = anim } ! [ Cmd.map DiagramLayoutMsg animCmd ]

        Resize size ->
            { model | size = size } ! []

        FetchRemote ->
            { model | errorMsg = "Fetching..." } ! [ fetchData RemoteFetched ]

        RemoteFetched elmFiles ->
            update (SetGraph <| ElmFileGraph.fromFiles elmFiles) model

        DiagramLayoutMsg msg ->
            let
                ( d, dx ) =
                    DiagramLayout.update msg model.layout
            in
                { model | layout = d } ! [ Cmd.map DiagramLayoutMsg dx ]


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


defaultRules : Rules.Model n
defaultRules =
    Rules.addForEachRule (Rules.noIntersection 100)
        <| Rules.addForOneRule (Rules.snapToGrid 100)
        <| Rules.empty
