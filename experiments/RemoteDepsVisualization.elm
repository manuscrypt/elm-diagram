module RemoteDepsVisualization exposing (..)

--
-- import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
-- import ElmFileGraph exposing (fromFiles)
-- import Html exposing (Html)
-- import Html.App as App
-- import Html.Attributes as HA
-- import Svg exposing (Svg)
-- import Http
-- import Task
--
-- import Task.Extra as Task
--

import Visualization
import Model.ElmFile as ElmFile exposing (ElmFile, decodeList)
import Dict exposing (Dict)
import Window


type alias Model =
    { visualizations : Dict Int (Visualization.Model ElmFile ())
    , message : String
    , size : Window.Size
    }



--
--
-- type Msg
--     = DataFetched (List ElmFile)
--     | ErrorOccurred Http.Error
--     | VisualizationMsg Int Visualization.Msg
--     | SetWindowSize Window.Size
--
--
-- main : Program Never
-- main =
--     App.program
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = (\model -> Sub.batch [ Window.resizes SetWindowSize ])
--         }
--
--
-- init : ( Model, Cmd Msg )
-- init =
--     let
--         m0 =
--             { visualizations = Dict.empty, message = "initializing...", size = { width = 0, height = 0 } }
--     in
--         m0 ! [ Window.size |> Task.performFailproof (\s -> SetWindowSize s), fetchData ]
--
--
-- fetchData : Cmd Msg
-- fetchData =
--     Task.perform ErrorOccurred DataFetched
--         <| Http.get decodeList "http://localhost:3001"
--
--
-- fromGraphs : List (Graph ElmFile ()) -> Model -> ( Model, Cmd Msg )
-- fromGraphs graphs model =
--     let
--         count =
--             floor (sqrt (toFloat <| List.length graphs))
--
--         labelFn =
--             (\x -> x.label.name)
--
--         size' =
--             { width = model.size.width // count, height = model.size.height // count }
--
--         ( vis, visfxs ) =
--             List.unzip <| List.map (\g -> Visualization.init g size' labelFn) graphs
--
--         newViss =
--             Dict.fromList <| List.indexedMap (,) vis
--     in
--         { model
--             | visualizations = newViss
--             , message = "initialized with " ++ (toString <| List.length graphs) ++ " graphs"
--         }
--             ! (List.map2 (\( i, v ) eff -> Cmd.map (VisualizationMsg i) eff) (Dict.toList newViss) visfxs)
--
--
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         SetWindowSize size ->
--             { model | size = size } ! []
--
--         ErrorOccurred err ->
--             { model | message = toString err } ! []
--
--         DataFetched elmFiles ->
--             fromGraphs (Graph.stronglyConnectedComponents <| fromFiles elmFiles) model
--
--         -- let
--         --     g =
--         --         fromFiles elmFiles
--         -- in
--         --     case Graph.get (idFor "Basic.elm" elmFiles) g of
--         --         Nothing ->
--         --             Debug.crash "no Basic.elm found"
--         --
--         --         Just ctx ->
--         --             let
--         --                 induced =
--         --                     Debug.log "oh"
--         --                         <| Graph.inducedSubgraph ([ ctx.node.id ] ++ (IntDict.keys ctx.outgoing)) g
--         --             in
--         --                 fromGraph induced model.size
--         VisualizationMsg id msg ->
--             case Dict.get id model.visualizations of
--                 Nothing ->
--                     model ! []
--
--                 Just vis ->
--                     let
--                         ( vis', eff ) =
--                             Visualization.update msg vis
--
--                         updated =
--                             Dict.update id (\mbVis -> Just vis') model.visualizations
--                     in
--                         ( { model | visualizations = updated }, Cmd.map (VisualizationMsg id) eff )
--
--
-- idFor : String -> List ElmFile -> NodeId
-- idFor str allFiles =
--     List.filter (\e -> e.name == str) allFiles |> List.head |> Maybe.map (\e -> e.id) |> Maybe.withDefault -1
--
--
-- view : Model -> Html Msg
-- view model =
--     Html.div [ bodyStyle ]
--         [ Html.div [] [ Html.text <| "Message: " ++ toString model.message ]
--         , Html.div [ HA.style [ (,) "display" "flex", (,) "flex-direction" "column" ] ]
--             [ Html.div [ HA.style [ (,) "flex" "auto" ] ] (Dict.values <| Dict.map viewVis model.visualizations)
--             ]
--         , Html.div
--             [ HA.style
--                 [ (,) "clear" "both"
--                 , (,) "position" "relative"
--                 ]
--             ]
--             [ Html.text <| toString model ]
--         ]
--
--
-- viewVis : Int -> Visualization.Model ElmFile () -> Svg Msg
-- viewVis id vis =
--     App.map (VisualizationMsg id) (Visualization.view vis)
--
--
-- bodyStyle : Html.Attribute a
-- bodyStyle =
--     HA.style
--         [ -- (,) "width" "920px"
--           -- , (,) "margin" "auto"
--           -- , (,) "border" "1px solid black"
--           (,) "background-color" "#EEEEEE"
--         }
