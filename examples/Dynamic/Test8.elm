module Dynamic.Test8 exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import Html.App as App
import VirtualDom 
import Http
import Task
import IntDict 
import Model.ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (fromFiles)

type alias Model =
    { graph : Graph ElmFile ()
    , labelFn : Node ElmFile -> String
    , error : String
    }


type Msg
    = DataFetched (List ElmFile)
    | ErrorOccurred Http.Error


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

    in
        { graph = g
        , labelFn = labelFn
        , error = "no error"
        }
            ! [ fetchData ]


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

            in
                { model | graph = newGraph }
                    ! [  ]

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

viewImport : Graph ElmFile () -> Int -> VirtualDom.Node a
viewImport graph importId =
    let impCtx = Debug.log "impCtx" <| Graph.get importId graph
    in case ( Graph.get importId graph ) of
        Just ctx -> viewNodeCtx graph ctx
        Nothing -> Html.text "import not found"  
    
viewNodeCtx : Graph ElmFile () -> Graph.NodeContext ElmFile () -> VirtualDom.Node a
viewNodeCtx graph ctx =
    let xxctx = Debug.log "ctx22222" <| ctx 
        outs = Debug.log "outs" <| ctx.outgoing                  
        importIds = Debug.log "importIds" <| IntDict.keys ctx.outgoing                  
    in
    Html.div [] 
    [ Html.div [] [ Html.text ( ctx.node.label.moduleName ++ " in " ++ ctx.node.label.path ) ]
    , Html.div 
        [ HA.style [ ( "padding-left", "20px" ) ] ]
        ( List.map ( \importId -> viewImport graph importId ) importIds )
    ]


viewNode : Graph ElmFile () -> ( Graph.Node ElmFile ) -> VirtualDom.Node a
viewNode graph node =
    let n = Debug.log "node" node
        -- tt = Debug.log "xx" <| iWantToWearShoes graph node
        ctx = Debug.log "ctx" <| case ( Graph.get node.id graph ) of 
            Just v -> v
            Nothing -> Debug.crash "ups"
        outs = Debug.log "outs" <| ctx.outgoing                  
        importIds = Debug.log "importIds" <| IntDict.keys ctx.outgoing                  
    in 
    Html.div [] 
    [ Html.div [] [ Html.text ( node.label.moduleName ++ " in " ++ node.label.path ) ]
    , Html.div 
        [ HA.style [ ( "padding-left", "20px" ) ] ]
        ( List.map ( \importId -> viewImport graph importId ) importIds )
        --[ Html.text ( node.label.moduleName ++ " in " ++ node.label.path ) ]
    ]



viewGraph : Graph ElmFile () -> VirtualDom.Node a
viewGraph graph = 
    let rootNodes = List.filter ( \n -> n.label.moduleName == "Basic" ) <|  Graph.nodes graph
    in
    Html.div [] ( List.map ( \n -> viewNode graph n ) rootNodes ) 

view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text <| "Error " ++ toString model.error ]
        , viewGraph model.graph 
        ]
