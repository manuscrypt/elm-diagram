module Model.Layout exposing (..)

import Graph exposing (Graph, NodeContext, Node, Edge)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, fromTuple)
import Model.ElmFile as ElmFile exposing (ElmFile)
import Random exposing (Generator, pair, float, Seed, step)


type alias PosAndLabel =
    ( Vec2, String )


type alias Model =
    { graph : Graph PosAndLabel ()
    , seed : Random.Seed
    }


type Msg
    = Animate


init : Graph ElmFile () -> Vec2 -> Seed -> ( Model, Cmd Msg )
init elmFileGraph size seed =
    let
        ( nodes, nextSeed ) =
            List.foldl (randomlyPlacedNode size) ( [], seed ) <| Graph.nodes elmFileGraph
    in
        { graph = Graph.fromNodesAndEdges nodes (Graph.edges elmFileGraph)
        , seed = nextSeed
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate ->
            model ! []


randomPoint : Vec2 -> Generator ( Float, Float )
randomPoint size =
    pair (float 0 <| getX size) (float 0 <| getY size)


randomlyPlacedNode : Vec2 -> Node ElmFile -> ( List (Node PosAndLabel), Seed ) -> ( List (Node PosAndLabel), Seed )
randomlyPlacedNode size node ( list, seed ) =
    let
        ( p, nextSeed ) =
            makePoint size seed
    in
        ( list ++ [ Node node.id ( p, node.label.moduleName ) ], nextSeed )


makePoint : Vec2 -> Seed -> ( Vec2, Seed )
makePoint size seed =
    let
        ( p, s ) =
            step (randomPoint size) seed
    in
        ( fromTuple p, s )
