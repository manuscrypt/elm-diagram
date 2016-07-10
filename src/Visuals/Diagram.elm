module Visuals.Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Dict exposing (insert)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Extra.Cmd exposing (noFx)
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Visuals.Grid as Grid


type alias Model =
    { size : Vec2
    , grid : Grid.Model
    , symbols : Dict.Dict Int Symbol.Model
    , connections : List Connection.Model
    }


type Msg
    = AddSymbol (Symbol.Model)
    | Connect (List Int)
    | Modify Int (Symbol.Msg)
    | NoOp


init : Vec2 -> List Symbol.Model -> List Connection.Model -> ( Model, Cmd Msg )
init size syms conns =
    let
        w =
            (getX size)

        h =
            (getY size)
    in
        { size = vec2 w h
        , grid = Grid.init ( 0, w ) ( 0, h ) 25
        , symbols = Dict.fromList <| List.map (\r -> ( r.id, r )) syms
        , connections = conns
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

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
                { model | connections = model.connections ++ [ Connection.init syms ] } ! []

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


view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        let
            symbols =
                List.map viewSymbol <| Dict.toList model.symbols

            connections =
                List.map Connection.view model.connections

            grid =
                Grid.view model.grid
        in
            Svg.svg
                [ SA.version "1.1"
                , SA.width sw
                , SA.height sh
                , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
                , SA.textRendering "optimizeLegibility"
                ]
                ([ grid ] ++ symbols ++ connections)
