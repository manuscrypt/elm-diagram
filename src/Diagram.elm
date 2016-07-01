module Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Dict exposing (insert)
import Symbol
import Connection
import Svg exposing (Svg)
import Svg.Attributes as SA
import Extra.Cmd exposing (noFx)

type alias Model =
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
    Connection.view con


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
            , SA.textRendering "optimizeLegibility"
            ] ( symbols ++ connections )
