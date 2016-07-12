module Visuals.Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Svg exposing (Svg)
import Svg.Attributes as SA
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Visuals.Grid as Grid
import Graph exposing (Graph, Node, NodeId, Edge)
import Model.Layout as Layout exposing (PosAndLabel)
import IntDict


type alias Model =
    { size : Vec2
    , grid : Grid.Model
    , graph : Graph Symbol.Model Connection.Model
    }


type Msg
    = AddSymbol Symbol.Model
    | Connect Symbol.Model Symbol.Model
    | Modify Int Symbol.Msg
    | SetPositions (Graph PosAndLabel ())
    | NoOp


init : Vec2 -> Graph PosAndLabel () -> ( Model, Cmd Msg )
init size g =
    let
        w =
            (getX size)

        h =
            (getY size)
    in
        { size = vec2 w h
        , grid = Grid.init ( 0, w ) ( 0, h ) 25
        , graph = graphFromPosGraph g
        }
            ! []


graphFromPosGraph : Graph PosAndLabel () -> Graph Symbol.Model Connection.Model
graphFromPosGraph g =
    let
        nodes =
            List.map toSymbol <| Graph.nodes g

        conns =
            List.map (toConnection nodes) <| Graph.edges g
    in
        Graph.fromNodesAndEdges nodes conns


toSymbol : Node PosAndLabel -> Node Symbol.Model
toSymbol { id, label } =
    let
        ( nodePos, nodeLabel ) =
            label
    in
        Node id (fst <| Symbol.init id nodeLabel nodePos)


toConnection : List (Node Symbol.Model) -> Edge () -> Edge Connection.Model
toConnection symbols edge =
    let
        filtered =
            List.map .label
                <| List.filter (\s -> List.member s.id [ edge.from, edge.to ]) symbols
    in
        Edge edge.from edge.to <| Connection.init filtered


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        AddSymbol symbol ->
            case Graph.get symbol.id model.graph of
                Nothing ->
                    model ! []

                Just ctx ->
                    let
                        newNode =
                            { node = Node (Graph.size model.graph) symbol
                            , incoming = IntDict.empty
                            , outgoing = IntDict.empty
                            }
                    in
                        { model | graph = Graph.insert newNode model.graph } ! []

        Connect fromSymbol toSymbol ->
            model ! []

        -- let
        --     edge =
        --         Edge fromSymbol.id toSymbol.id <| Connection.init [ fromSymbol, toSymbol ]
        -- in
        --     case Graph.get toSymbol.id model.graph of
        --         Nothing ->
        --             model ! []
        --
        --         Just ctx ->
        --             let
        --                 newCtx =
        --                     { ctx | incoming = IntDict.insert fromSymbol.id edge ctx.incoming }
        --             in
        --                 { model | graph = Graph.update toSymbol.id (always <| Just newCtx) model.graph } ! []
        Modify id msg ->
            case Graph.get id model.graph of
                Nothing ->
                    model ! []

                Just ctx ->
                    let
                        ( sym', eff ) =
                            Symbol.update msg ctx.node.label

                        newCtx =
                            { ctx | node = Node id sym' }
                    in
                        ( { model | graph = Graph.update id (always <| Just newCtx) model.graph }
                        , Cmd.map (Modify id) eff
                        )

        SetPositions graph ->
            --{ model | graph = graphFromPosGraph graph } ! []
            let
                msgs =
                    List.map (\n -> Modify n.id (Symbol.Move <| fst n.label)) <| Graph.nodes graph
            in
                List.foldl moveSymbol (model ! []) msgs



--updateMany msgs update model ! []


moveSymbol : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveSymbol msg ( model, oldCmd ) =
    let
        ( model', cmd ) =
            update msg model
    in
        model' ! [ cmd, oldCmd ]


viewSymbol : Symbol.Model -> Svg Msg
viewSymbol sym =
    App.map (Modify sym.id) (Symbol.view sym)


symbols : { c | graph : Graph a b } -> List a
symbols model =
    List.map .label <| Graph.nodes model.graph


connections : { c | graph : Graph a b } -> List b
connections model =
    List.map .label <| Graph.edges model.graph


view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        Svg.svg
            [ SA.version "1.1"
            , SA.width sw
            , SA.height sh
            , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
            , SA.textRendering "optimizeLegibility"
            ]
            ([ Grid.view model.grid ]
                ++ (List.map viewSymbol <| symbols model)
                ++ (List.map Connection.view <| connections model)
            )
