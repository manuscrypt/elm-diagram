module Dynamic.Test9 exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import Html.App as App
import VirtualDom
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Http
import Task
import IntDict
import Model.ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (fromFiles)
import Dynamic.Diagram as DynamicDiagram
import Layouts.Rules as DynamicLayout
import Time exposing (Time, second)


type alias Model =
    { graph : Graph ElmFile ()
    , labelFn : Node ElmFile -> String
    , error : String
    , diagram : DynamicDiagram.Model
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


init : ( Model, Cmd Msg )
init =
    let
        g =
            Graph.empty

        labelFn =
            (\x -> x.label.name ++ " (" ++ x.label.moduleName ++ ")")
    in
        { graph = g
        , labelFn = labelFn
        , error = "no error"
        , diagram = DynamicDiagram.init
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
            { model | diagram = DynamicDiagram.animate model.diagram } ! []

        ErrorOccurred err ->
            { model | error = toString err } ! []

        DataFetched elmFiles ->
            let
                newGraph =
                    ElmFileGraph.fromFiles elmFiles

                newDiagram =
                    loadDiagram newGraph model.diagram
            in
                { model
                    | graph = newGraph
                    , diagram = newDiagram
                }
                    ! []



{-

   --fff : Graph.NodeContext a b -> c -> ( c, c -> c )
   --fff ctx list = a

   iWantToWearShoes :  Graph ElmFile () -> Graph.Node ElmFile -> List Int
   iWantToWearShoes graph startNode =
                   (graph
                   |> Graph.bfs (Graph.ignorePath (::)) []
                   |> List.map (.node >> .id)
                   |> List.reverse)


     Graph.guidedDfs
       Graph.alongIncomingEdges            -- which edges to follow
       fff -- (Graph.onDiscovery (\ctx list ->    -- append node labels on discovery
         -- ctx.node.label.name :: list))
       [ startNode.id ]            -- start with the node labelled "Shoes"
       []                                  -- accumulate starting with the empty list
       graph                             -- traverse our dressUp graph from above
       |> fst                              -- ignores the untraversed rest of the graph
-}


n2dn : Graph.Node ElmFile -> DynamicDiagram.Node
n2dn node =
    { name = node.label.name }


ctx2dn : Graph.NodeContext ElmFile () -> DynamicDiagram.Node
ctx2dn ctx =
    n2dn ctx.node


loadImport : Graph ElmFile () -> Graph.NodeContext ElmFile () -> Int -> DynamicDiagram.Model -> DynamicDiagram.Model
loadImport graph sourceCtx importId diagram =
    let
        impCtx =
            Debug.log "impCtx" <| Graph.get importId graph
    in
        case (Graph.get importId graph) of
            Just trgtCtx ->
                (DynamicDiagram.addImport (ctx2dn sourceCtx)
                    (ctx2dn trgtCtx)
                    (if (DynamicDiagram.containsNode (ctx2dn trgtCtx) diagram) then
                        diagram
                     else
                        viewNodeCtx graph trgtCtx diagram
                    )
                )

            Nothing ->
                Debug.crash "import not found"


viewNodeCtx : Graph ElmFile () -> Graph.NodeContext ElmFile () -> DynamicDiagram.Model -> DynamicDiagram.Model
viewNodeCtx graph ctx diagram =
    let
        xxctx =
            Debug.log "ctx22222" <| ctx

        outs =
            Debug.log "outs" <| ctx.outgoing

        importIds =
            Debug.log "importIds" <| IntDict.keys ctx.outgoing
    in
        List.foldl (\importId d -> loadImport graph ctx importId d)
            (DynamicDiagram.addNode (ctx2dn ctx) diagram)
            importIds



--


loadNode : Graph ElmFile () -> Graph.Node ElmFile -> DynamicDiagram.Model -> DynamicDiagram.Model
loadNode graph node diagram =
    let
        ctx =
            Debug.log "ctx"
                <| case (Graph.get node.id graph) of
                    Just v ->
                        v

                    Nothing ->
                        Debug.crash "ups"

        outs =
            Debug.log "outs" <| ctx.outgoing

        importIds =
            Debug.log "importIds" <| IntDict.keys ctx.outgoing
    in
        List.foldl (\importId d -> loadImport graph ctx importId d)
            (DynamicDiagram.addRootNode (n2dn node) diagram)
            importIds


loadDiagram : Graph ElmFile () -> DynamicDiagram.Model -> DynamicDiagram.Model
loadDiagram graph diagram =
    let
        rootNodes =
            List.filter (\n -> n.label.moduleName == "Basic") <| Graph.nodes graph
    in
        List.foldl (\n d -> loadNode graph n d) diagram rootNodes


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
                , viewBox (DynamicLayout.viewboxAsString 100 model.diagram.layout)
                ]
                (DynamicDiagram.view model.diagram)
            ]
        ]
