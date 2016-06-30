module Dependencies exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as HA
import Html.Events as HE
import Http 
import Task 
import IntDict 
import String 
import ElmFile exposing (ElmFile, decodeList) 

type alias Model 
    = { graph : Graph ElmFile ()
      , selectedId: Maybe NodeId 
      , hoveringId: Maybe NodeId 
      , error: String
      }

type Msg 
    = NoOp
    | DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | SelectNode NodeId
    | Hover NodeId
    | Unhover

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

init: (Model, Cmd Msg)
init = { graph = Graph.empty
       , selectedId = Nothing 
       , hoveringId = Nothing 
       , error = "" 
       } ! [ fetchData ] 

fetchData : Cmd Msg
fetchData =
    Task.perform ErrorOccurred DataFetched <|
        Http.get decodeList "http://localhost:3001"

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        NoOp -> 
            model ! []
        ErrorOccurred err -> 
            { model | error = toString err } ! []
        DataFetched elmFiles -> 
            { model | graph = fromFiles elmFiles } ! []
        SelectNode nodeId ->
            { model | selectedId = Just nodeId } ! []
        Hover nodeId ->
            { model | hoveringId = Just nodeId } ! []
        Unhover ->
            { model | hoveringId = Nothing } ! []

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
    Html.div [] [ Html.div [] [ Html.text model.error ]  
                , Html.div [ flexHoriz ] [ nodeList model
                                         , nodeDetail model
                                         ]
                ]
    -- Html.div [] [ Html.text <| toString <| Graph.topologicalSort model.graph 
    --             , Html.text <| Graph.toString' model.graph
    --             , Html.div [] [Html.text model.error ]]


nodeList model = 
    let nodes = Graph.nodes model.graph
    in Html.div [flexVert] (List.map (nodeCard model.selectedId) nodes)

nodeCard selectedId node = 
    Html.div [ cardStyle node selectedId
             , HE.onClick (SelectNode node.id) 
             , HE.onMouseOver (Hover node.id) 
             , HE.onMouseOut Unhover 
             ] [ Html.text <| (toString node.id) ++ ": " ++ node.label.name ]

nodeDetail model = 
    case model.selectedId of 
        Nothing ->
            Html.div [] [Html.text "nothing selected"]
        Just id ->
            case Graph.get id model.graph of 
                Nothing -> 
                    Html.div [] [Html.text <| "invalid node-id: " ++ (toString id) ]
                Just ctx ->
                    viewContext ctx

viewContext ctx = 
    Html.div [] [ Html.div [] [Html.text <| "Id: " ++ (toString ctx.node.id)]
                , Html.div [] [Html.text <| toString ctx.node.label ] 
                , Html.div [] [Html.text <| "in: " ++ (toString <| IntDict.size ctx.incoming) ] 
                , Html.div [] [Html.text <| "out: " ++ (toString <| IntDict.size ctx.outgoing) ] 
                ] 
    
cardStyle n sel =
    let bgColor =
        case sel of 
            Nothing -> "#FFFFFF"
            Just s -> 
                if( n.id == s ) then 
                    "#EEAAAA"
                else 
                    "#FFFFFF"

    in HA.style [ (,) "flex" "auto"
                , (,) "border-bottom" "1px solid gray"
                , (,) "background-color" bgColor
                ]

flexVert =
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "column" 
             , (,) "flex-wrap" "none" 
             ]

flexHoriz = 
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "row" 
             , (,) "flex-wrap" "none" 
             ]

    -- let nodes = [ Node 0 "Socks"
    --             , Node 1 "Undershirt"
    --             , Node 2 "Pants"
    --             , Node 3 "Shoes"
    --             , Node 4 "Watch"
    --             , Node 5 "Shirt"
    --             , Node 6 "Belt"
    --             , Node 7 "Tie"
    --             , Node 8 "Jacket"
    --             ]

    --     e from to = Edge from to ()

    --     edges =
    --         [ e 0 3 -- socks before shoes
    --         , e 1 2 -- undershorts before pants
    --         , e 1 3 -- undershorts before shoes
    --         , e 2 3 -- pants before shoes
    --         , e 2 6 -- pants before belt
    --         , e 5 6 -- shirt before belt
    --         , e 5 7 -- shirt before tie
    --         , e 6 8 -- belt before jacket
    --         , e 7 8 -- tie before jacket
    --         ]
    -- in { graph = Graph.fromNodesAndEdges nodes edges }
