module Visuals.HtmlTree exposing (..)

import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import Html exposing (Html)
import Html.Attributes as HA
import VirtualDom
import IntDict
import Model.ElmFile exposing (ElmFile, decodeList)
import Model.ElmFileGraph as ElmFileGraph exposing (ElmFileGraph, fromFiles)


viewImport : ElmFileGraph -> Int -> VirtualDom.Node a
viewImport graph importId =
    let
        impCtx =
            Debug.log "impCtx" <| Graph.get importId graph
    in
        case (Graph.get importId graph) of
            Just ctx ->
                viewNodeCtx graph ctx

            Nothing ->
                Html.text "import not found"


viewNodeCtx : ElmFileGraph -> Graph.NodeContext ElmFile () -> VirtualDom.Node a
viewNodeCtx graph ctx =
    let
        xxctx =
            Debug.log "ctx22222" <| ctx

        outs =
            Debug.log "outs" <| ctx.outgoing

        importIds =
            Debug.log "importIds" <| IntDict.keys ctx.outgoing
    in
        Html.div []
            [ Html.div [] [ Html.text (ctx.node.label.moduleName ++ " in " ++ ctx.node.label.path) ]
            , Html.div [ HA.style [ ( "padding-left", "20px" ) ] ]
                (List.map (\importId -> viewImport graph importId) importIds)
            ]


viewNode : ElmFileGraph -> Graph.Node ElmFile -> VirtualDom.Node a
viewNode graph node =
    let
        n =
            Debug.log "node" node

        -- tt = Debug.log "xx" <| iWantToWearShoes graph node
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
        Html.div []
            [ Html.div [] [ Html.text (node.label.moduleName ++ " in " ++ node.label.path) ]
            , Html.div [ HA.style [ ( "padding-left", "20px" ) ] ]
                (List.map (\importId -> viewImport graph importId) importIds)
              --[ Html.text ( node.label.moduleName ++ " in " ++ node.label.path ) ]
            ]


viewGraph : ElmFileGraph -> VirtualDom.Node a
viewGraph graph =
    let
        rootNodes =
            List.filter (\n -> n.label.moduleName == "Basic") <| Graph.nodes graph
    in
        Html.div [] (List.map (\n -> viewNode graph n) rootNodes)



-- view : Model -> Html Msg
-- view model =
--     Html.div []
--         [ Html.div [] [ Html.text <| "Error " ++ toString model.error ]
--         , viewGraph model.graph
--         ]
