module Model.DiagramLayout exposing (..)

-- import Layouts.Rules as RulesLayout
--     exposing
--         ( addForEachRule
--         , addForOneRule
--         , addForTwoRule
--         , addNodeAtPosition
--         , noIntersection
--         , snapToGrid
--         , hasSameX
--         , hasSameY
--         , isAbove
--         )

import Graph exposing (Graph, NodeContext, Node, Edge)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Html.App as App
import Math.Vector2 exposing (Vec2, vec2)
import Model.Layout as Layout
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Visuals.Diagram as Diagram


-- MODEL


type alias Model =
    { graph : Graph Symbol.Model Connection.Model
    , rootNodes : List Symbol.Model
    , layout : Layout.Model
    , diagram : Diagram.Model
    }


type Msg
    = NoOp
    | Animate
    | DiagramMsg Diagram.Msg


init : Layout.Model -> ( Model, Cmd Msg )
init layout =
    let
        graph =
            convert layout.graph

        ( dg, dgCmd ) =
            Diagram.init (vec2 500 500) (List.map .label <| Graph.nodes graph) (List.map .label <| Graph.edges graph)
    in
        { rootNodes = []
        , layout = layout
        , graph = graph
        , diagram = dg
        }
            ! [ Cmd.map DiagramMsg dgCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Animate ->
            { model
                | layout = Layout.animate model.layout
            }
                ! []

        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )


view : Model -> Html Msg
view model =
    diagram model


diagram : Model -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            , (,) "border" "1px solid violet"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


convert : Graph ( Vec2, String ) () -> Graph Symbol.Model Connection.Model
convert g =
    let
        nodes =
            List.map toSymbol <| Graph.nodes g

        edges =
            List.map (toConnection nodes) <| Graph.edges g
    in
        Graph.fromNodesAndEdges nodes edges


toSymbol : Node ( Vec2, String ) -> Node Symbol.Model
toSymbol node =
    Node node.id (fst <| Symbol.init node.id (snd node.label) (fst node.label))


toConnection : List (Node Symbol.Model) -> Edge b -> Edge Connection.Model
toConnection nodes edge =
    case List.filter (\n -> List.member n.id [ edge.from, edge.to ]) nodes of
        [ from, to ] ->
            Edge from.id to.id <| Connection.init <| [ from.label, to.label ]

        _ ->
            Debug.crash "invalid edge"



-- addRootNode : Node -> Model -> Model
-- addRootNode rootNode model =
--     { model
--         | rootNodes = model.rootNodes ++ [ rootNode ]
--         , nodes = model.nodes ++ [ rootNode ]
--         , layout =
--             addNodeAtPosition rootNode (vec2 (toFloat (List.length model.nodes) * 9) 100)
--                 <| List.foldr (\otherRoot -> addForTwoRule otherRoot (hasSameY 0.05) rootNode) model.layout model.rootNodes
--     }
--
--
-- addNode : Node -> Model -> Model
-- addNode node model =
--     { model
--         | nodes =
--             model.nodes ++ [ node ]
--             --, layout = addNode node model.layout
--         , layout = addNodeAtPosition node (vec2 (toFloat (List.length model.nodes) * 10) 0) model.layout
--     }
--
--
-- addImport : Node -> Node -> Model -> Model
-- addImport parent child model =
--     { model
--         | connections = model.connections ++ [ ( parent, child ) ]
--         , layout =
--             addForTwoRule child (isAbove 100) parent
--                 <| addForTwoRule child
--                     (hasSameX 0.0005)
--                     parent
--                     model.layout
--     }
-- svgNodes : Model -> List (VirtualDom.Node a)
-- svgNodes model =
--     (Dynamic.SvgVisualization.grid (viewbox 50 model.layout))
--         ++ (List.concat
--                 <| List.map
--                     (\( nodeA, nodeB ) ->
--                         Dynamic.SvgVisualization.connection (positionOfNode nodeB model.layout)
--                             (positionOfNode nodeA model.layout)
--                     )
--                     model.connections
--            )
--         ++ (List.concat
--                 <| List.map
--                     (\node ->
--                         Dynamic.SvgVisualization.node (positionOfNode node model.layout)
--                             node.name
--                     )
--                     model.nodes
--            )
