module Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Dict exposing (insert)
import Symbol
import Connection
import Svg exposing (Svg)
import Svg.Attributes as SA
import Extra.Cmd exposing (noFx)
import Layout
import DependencyGraph exposing (Edge, Vertex)
import Tuple2

type alias Model=
    { size : Vec2
    , gridSize : Vec2
    , symbols : Dict.Dict Int Symbol.Model
    , connections : List Connection.Model
    }


type Msg
    = AddSymbol (Symbol.Model)
    | Connect (List Int)
    | Modify Int (Symbol.Msg)
    | NoOp

init : List Symbol.Model -> List Connection.Model -> ( Model, Cmd Msg )
init syms conns =
        { size = vec2 800 600
        , gridSize = vec2 10 10
        , symbols = Dict.fromList <| List.map (\r -> (r.id,r)) syms
        , connections = conns
        } ! []

createSymbols: List Layout.LayoutNode -> (List  Symbol.Model, List (Cmd Symbol.Msg))
createSymbols nodes = 
    List.unzip <| List.indexedMap (\i node -> Symbol.init i node.color (vec2 20 20) node.pos ) nodes 

createConnection: List Symbol.Model -> Layout.Model a -> Edge a -> Connection.Model
createConnection symbols layout edge = 
    let cells = edgeToCells edge layout 
        syms = cellsToSymbols cells symbols
    in Connection.init syms 


cellsToSymbols: List (Layout.LayoutCell a) -> List Symbol.Model -> List Symbol.Model
cellsToSymbols cells symbols =
    let idxs = List.map .index cells
    in List.filter (\s -> List.member s.id idxs) symbols 

edgeToCells: Edge a -> Layout.Model a -> List (Layout.LayoutCell a)
edgeToCells e layout =
    let vs = Tuple2.toList e
    in List.filterMap (Layout.getCell layout) vs

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> model ! []
        AddSymbol symbol ->
            let
                newSym =
                    insert symbol.id symbol model.symbols
            in
                { model | symbols = newSym } ! []

        Connect ints ->
            let
                syms =
                    List.filterMap (\i -> Dict.get i model.symbols) ints

            in
                { model | connections = model.connections ++ [Connection.init syms] } ! []

        Modify id msg ->
            case Dict.get id model.symbols of
                Nothing ->
                    noFx model

                Just sym ->
                    let
                        ( sym', eff ) =
                            Symbol.update msg sym

                        updatedSymbols =
                            Dict.update id (\mbSym -> Just sym') model.symbols
                    in
                        ( { model | symbols = updatedSymbols }, Cmd.map (Modify id) eff )


viewSymbol : ( Int, Symbol.Model ) -> Svg Msg
viewSymbol ( id, sym ) =
    App.map (Modify id) (Symbol.view sym)


viewConnection : Connection.Model -> Svg Msg
viewConnection con =
    App.map (\_ -> NoOp) (Connection.view con)


view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        let symbols = List.map viewSymbol <| Dict.toList model.symbols
            connections = List.map viewConnection model.connections 
        in Svg.svg
            [ SA.width sw
            , SA.height sh
            , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
            ] ( symbols ++ connections )
