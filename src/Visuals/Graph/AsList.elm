module Visuals.Graph.AsList exposing (..)

import Graph exposing (Graph, Node, NodeContext, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import IntDict


type alias Model a =
    { labelFunc : Node a -> String
    , selectedId : Maybe NodeId
    , hoveringId : Maybe NodeId
    }


type Msg
    = SelectNode NodeId
    | Hover NodeId
    | Unhover


init : (Node a -> String) -> ( Model a, Cmd Msg )
init labelFunc =
    { labelFunc = labelFunc
    , selectedId = Nothing
    , hoveringId = Nothing
    }
        ! []


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        SelectNode nodeId ->
            { model | selectedId = Just nodeId } ! []

        Hover nodeId ->
            { model | hoveringId = Just nodeId } ! []

        Unhover ->
            { model | hoveringId = Nothing } ! []


view : Model a -> Graph a b -> Html Msg
view model graph =
    let
        nodes =
            Graph.nodes graph
    in
        Html.div
            [ flexHoriz
            ]
            [ Html.div
                [ flexVert
                ]
              <|
                (List.map (nodeCard model) nodes)
                    ++ [ nodeDetail model graph ]
            ]


nodeCard : Model a -> Node a -> Html Msg
nodeCard model node =
    Html.div
        [ cardStyle node model
        , HE.onClick (SelectNode node.id)
        , HE.onMouseOver (Hover node.id)
        , HE.onMouseOut Unhover
        ]
        [ Html.text <| (toString node.id) ++ ": " ++ (model.labelFunc node) ]


nodeDetail : Model a -> Graph a b -> Html Msg
nodeDetail model graph =
    let
        content =
            case model.selectedId of
                Nothing ->
                    [ Html.text "nothing selected" ]

                Just id ->
                    case Graph.get id graph of
                        Nothing ->
                            [ Html.text <| "invalid node-id: " ++ (toString id) ]

                        Just ctx ->
                            [ Html.div [ bottomBordered ] [ Html.text <| "Id: " ++ (toString ctx.node.id) ++ ", Label: " ++ toString ctx.node.label ]
                            , Html.div [ bottomBordered ]
                                [ Html.text <| "in: " ++ (toString <| IntDict.size ctx.incoming)
                                , viewAdjacent ctx.incoming model graph
                                ]
                            , Html.div [ bottomBordered ]
                                [ Html.text <| "out: " ++ (toString <| IntDict.size ctx.outgoing)
                                , viewAdjacent ctx.outgoing model graph
                                ]
                            ]
    in
        Html.div [ HA.style [ (,) "width" "100%", (,) "padding" "20px" ] ] content


viewAdjacent : Adjacency b -> Model a -> Graph a b -> Html Msg
viewAdjacent adj model graph =
    let
        ctxs =
            List.filterMap (\e -> Graph.get e graph) <| IntDict.keys adj
    in
        Html.div [] (List.map (nodeCard model << .node) ctxs)


bottomBordered : Html.Attribute a
bottomBordered =
    HA.style [ (,) "border-bottom" "1px solid black", (,) "padding" "4px" ]


selectStyle : Node a -> Model a -> List ( String, String )
selectStyle node model =
    case model.selectedId of
        Nothing ->
            []

        Just id ->
            if (id == node.id) then
                [ (,) "border" "2px solid black" ]
            else
                []


hoverStyle : Node a -> Model a -> List ( String, String )
hoverStyle node model =
    case model.hoveringId of
        Nothing ->
            []

        Just id ->
            if (id == node.id) then
                [ (,) "background-color" "#EEAAAA" ]
            else
                []


cardStyle : Node a -> Model a -> Html.Attribute Msg
cardStyle n model =
    HA.style
        ([ (,) "flex" "auto"
         , (,) "border-bottom" "1px solid gray"
         , (,) "cursor" "pointer"
         ]
            ++ (selectStyle n model)
            ++ (hoverStyle n model)
        )


flexVert : Html.Attribute Msg
flexVert =
    HA.style
        [ (,) "display" "flex"
        , (,) "flex-direction" "column"
        , (,) "flex-wrap" "none"
        , (,) "flex" "0 0 15%"
        ]


flexHoriz : Html.Attribute Msg
flexHoriz =
    HA.style
        [ (,) "display" "flex"
        , (,) "flex-direction" "row"
        , (,) "flex-wrap" "none"
        , (,) "margin" "10px"
        , (,) "padding" "20px"
        ]
