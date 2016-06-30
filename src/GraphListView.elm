module GraphListView exposing (..)

import Graph exposing (Graph, Node, NodeContext, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import IntDict 

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
    in Html.div [flexVert] (List.map (nodeCard model.selectedId model.labelFunc) nodes)

nodeCard: Maybe NodeId -> (Node a->String) -> Node a  -> Html Msg
nodeCard selectedId labelFunc node = 
    Html.div [ cardStyle node selectedId
             , HE.onClick (SelectNode node.id) 
             , HE.onMouseOver (Hover node.id) 
             , HE.onMouseOut Unhover 
             ] [ Html.text <| (toString node.id) ++ ": " ++ (labelFunc node) ]

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
                    viewContext ctx

viewContext: NodeContext a b -> Html Msg
viewContext ctx = 
    Html.div [] [ Html.div [] [Html.text <| "Id: " ++ (toString ctx.node.id)]
                , Html.div [] [Html.text <| toString ctx.node.label ] 
                , Html.div [] [Html.text <| "in: " ++ (toString <| IntDict.size ctx.incoming) ] 
                , Html.div [] [Html.text <| "out: " ++ (toString <| IntDict.size ctx.outgoing) ] 
                ] 
    
cardStyle: Node a -> Maybe NodeId -> Html.Attribute Msg
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
flexVert : Html.Attribute Msg
flexVert =
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "column" 
             , (,) "flex-wrap" "none" 
             ]
flexHoriz : Html.Attribute Msg
flexHoriz = 
    HA.style [ (,) "display" "flex" 
             , (,) "flex-direction" "row" 
             , (,) "flex-wrap" "none" 
             ]