module GraphListView exposing (..)

import Graph exposing (Graph, Node, NodeContext, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import IntDict 
import Html.App as App
import BasicElmDeps


type alias Model a b 
    = { graph: Graph a b
      , labelFunc: (Node a -> String)
      , selectedId: Maybe NodeId 
      , hoveringId: Maybe NodeId 
      }

type Msg
    = SelectNode NodeId
    | Hover NodeId
    | Unhover

init: Graph a b -> (Node a -> String) -> (Model a b, Cmd Msg)
init g labelFunc = 
    { graph = g
    , labelFunc = labelFunc
    , selectedId = Nothing     
    , hoveringId = Nothing
    } ! []

update: Msg -> Model a b -> (Model a b, Cmd Msg)
update msg model =
    case msg of 
        SelectNode nodeId ->
            { model | selectedId = Just nodeId } ! []
        Hover nodeId ->
            { model | hoveringId = Just nodeId } ! []
        Unhover ->
            { model | hoveringId = Nothing } ! []

view: Model a b -> Html Msg
view model =
    Html.div [ flexHoriz ] [ nodeList model, nodeDetail model ]

nodeList: Model a b -> Html Msg
nodeList model = 
    let nodes = Graph.nodes model.graph
    in Html.div [flexVert] (List.map (nodeCard model) nodes)

nodeCard: Model a b -> Node a -> Html Msg
nodeCard model node = 
    Html.div [ cardStyle node model
             , HE.onClick (SelectNode node.id) 
             , HE.onMouseOver (Hover node.id) 
             , HE.onMouseOut Unhover 
             ] [ Html.text <| (toString node.id) ++ ": " ++ (model.labelFunc node) ]

nodeDetail: Model a b -> Html Msg
nodeDetail model = 
    case model.selectedId of 
        Nothing ->
            Html.div [] [Html.text "nothing selected"]
        Just id ->
            case Graph.get id model.graph of 
                Nothing -> 
                    Html.div [] [Html.text <| "invalid node-id: " ++ (toString id) ]
                Just ctx ->
                    viewContext ctx model

viewContext: NodeContext a b -> Model a b -> Html Msg
viewContext ctx model = 
        Html.div [ HA.style [(,) "width" "100%"]] [ Html.div [] [Html.text <| "Id: " ++ (toString ctx.node.id)]
                , Html.div [] [Html.text <| toString ctx.node.label ] 
                , Html.div [] [Html.text <| "in: " ++ (toString <| IntDict.size ctx.incoming) ] 
                , viewAdjacent ctx.incoming model
                , Html.div [] [Html.text <| "out: " ++ (toString <| IntDict.size ctx.outgoing) ]
                , viewAdjacent ctx.outgoing model
                ] 

viewAdjacent: Adjacency b -> Model a b -> Html Msg
viewAdjacent adj model = 
    let ctxs = List.filterMap (\e -> Graph.get e model.graph) <| IntDict.keys adj
    in Html.div [] (List.map (nodeCard model << .node) ctxs)

selectStyle: Node a -> Model a b -> List (String,String)
selectStyle node model =
    case model.selectedId of 
        Nothing -> []
        Just id -> 
            if( id == node.id ) then 
                [ (,) "border" "2px solid black" ]
            else 
                []
hoverStyle: Node a -> Model a b -> List (String,String)
hoverStyle node model =
    case model.hoveringId of 
        Nothing -> []
        Just id -> 
            if( id == node.id ) then 
                [ (,) "background-color" "#EEAAAA" ]
            else 
                []

cardStyle: Node a -> Model a b -> Html.Attribute Msg
cardStyle n model =
    HA.style ([ (,) "flex" "auto"
             , (,) "border-bottom" "1px solid gray"
             , (,) "cursor" "pointer"
             ] ++ (selectStyle n model) ++ (hoverStyle n model)) 

flexVert : Html.Attribute Msg
flexVert =
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "column" 
             , (,) "flex-wrap" "none"
             , (,) "flex" "0 0 15%" 
             ]
flexHoriz : Html.Attribute Msg
flexHoriz = 
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "row" 
             , (,) "flex-wrap" "none" 
             ]

main : Program Never
main =
    let (g,fn) = BasicElmDeps.sample2
    in App.program
        { init = init g fn
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
