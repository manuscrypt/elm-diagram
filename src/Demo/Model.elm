module Demo.Model exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Http
import Extra.Http as Http
import Graph exposing (Graph, Node)
import Time exposing (Time)
import Window exposing (Size)
import Task
import Task.Extra as Task exposing (performFailproof)
import Random exposing (Seed, initialSeed)
import Date exposing (Date)
import Json.Decode as Json
import Result
import Maybe
import IntDict
import Graph exposing (Graph, Node, Edge, NodeId)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Model.ElmFile as ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph)
import Visuals.Graph.AsList as GraphListView
import Visuals.Diagram.Diagram as Diagram
import AnimationFrame


type Msg
    = Init Time
    | UpdateTime Time
    | ServerReachable Bool
    | SetErrorMsg String
    | Resize Size
    | TextInputChanged String
    | FetchRemote
    | SetGraph (ElmFileGraph)
    | RemoteFetched (List ElmFile)
    | Start
    | Stop
    | Animate Time
    | DiagramMsg Diagram.Msg
    | GraphListViewMsg GraphListView.Msg


type alias Model =
    { graph : ElmFileGraph
    , listView : GraphListView.Model ElmFile
    , graphString : String
    , diagram : Diagram.Model
    , lastDt : Time
    , size : Vec2
    , errorMsg : String
    , now : Maybe Date
    , seed : Seed
    , serverRunning : Bool
    , running : Bool
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.running then
            AnimationFrame.diffs Animate
          else
            Sub.none
        ]


init : ElmFileGraph -> ( Model, Cmd Msg )
init graph =
    let
        ( lv, lvCmd ) =
            GraphListView.init (\node -> node.label.moduleName)

        ( model, cmd ) =
            { graph = graph
            , listView = lv
            , diagram = Diagram.init graph
            , graphString = ""
            , lastDt = 0
            , size = vec2 0 0
            , errorMsg = ""
            , seed = initialSeed Random.minInt
            , now = Nothing
            , serverRunning = False
            , running = False
            }
                ! [ Time.now |> performFailproof Init
                  , checkServerStatus
                  , Cmd.map GraphListViewMsg lvCmd
                    --, fetchData RemoteFetched
                  ]

        ( model', cmd' ) =
            update (SetGraph graph) model
    in
        model' ! [ cmd, cmd' ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init time ->
          let (withTime, withTimeCmd) = update (UpdateTime time) { model |  seed = Random.initialSeed (round time) }
          in withTime ! [withTimeCmd, Window.size |> performFailproof Resize]

        UpdateTime time ->
              { model | now = Just <| Date.fromTime time} ! []


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
                    GraphListView.init (\x -> x.label.moduleName)
            in
                { model
                    | graph = graph
                    , graphString = ElmFileGraph.toString graph
                    , diagram =
                        Diagram.update (Diagram.Resize (Vec2.scale 0.5 model.size))
                            <| Diagram.init graph
                    , listView = lv
                    , errorMsg = "Graph set"
                }
                    ! [ Cmd.map GraphListViewMsg lvCmd ]

        Start ->
            { model | running = True } ! []

        Stop ->
            { model | running = False } ! []

        Animate dt ->
            update (DiagramMsg <| Diagram.Animate dt) { model | lastDt = dt }

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

        SetErrorMsg strErr ->
            { model | errorMsg = strErr } ! []

        Resize size ->
            let
                vecSize =
                    vec2 (toFloat size.width) (toFloat size.height)

                diagram =
                    Diagram.update (Diagram.Resize (Vec2.scale 0.5 vecSize)) model.diagram
            in
                { model
                    | size = vecSize
                    , diagram = diagram
                }
                    ! []

        FetchRemote ->
            { model | errorMsg = "Fetching..." } ! [ fetchData RemoteFetched ]

        RemoteFetched elmFiles ->
            update (SetGraph <| ElmFileGraph.fromFiles elmFiles) model

        DiagramMsg msg ->
            { model | diagram = Diagram.update msg model.diagram } ! []


serviceUrl: String
serviceUrl = "/deps"

fetchData : (List ElmFile -> Msg) -> Cmd Msg
fetchData msg =
    Task.perform (\err -> SetErrorMsg <| Http.httpErrorToString err) msg
        <| Http.get decodeList serviceUrl


checkServerStatus : Cmd Msg
checkServerStatus =
    Task.perform (\_ -> ServerReachable False) (\_ -> ServerReachable True)
        <| head serviceUrl


head : String -> Task.Task Http.RawError Http.Response
head url =
    Http.send Http.defaultSettings
        { verb = "HEAD"
        , headers = []
        , url = url
        , body = Http.empty
        }
