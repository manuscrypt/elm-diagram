module Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html.App as App
import Color exposing (Color)
import Dict exposing (insert)
import Symbol
import Connection
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import SvgUtil exposing (sx, sy)


--import Ball


type alias Model =
    { size : Vec2
    , gridSize : Vec2
    , symbols : Dict.Dict Int Symbol.Model
    , connections : Dict.Dict Int Connection.Model
    }


type Msg
    = Add Symbol.Shape Color Vec2 Vec2
    | Connect (List Int)
    | Modify Int Symbol.Msg
    | ModifyConnection Int Connection.Msg


init : ( Model, Cmd Msg )
init =
    noFx
        { size = vec2 800 600
        , gridSize = vec2 10 10
        , symbols = Dict.empty
        , connections = Dict.empty
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add shape color size pos ->
            let
                newId =
                    Dict.size model.symbols

                ( symbol, fx ) =
                    Symbol.init shape color size pos

                newSym =
                    insert newId symbol model.symbols
            in
                ( { model | symbols = newSym }, Cmd.map (Modify newId) fx )

        Connect ints ->
            let
                id =
                    (Dict.size model.connections)

                syms =
                    List.filterMap (\i -> Dict.get i model.symbols) ints

                ( c, cx ) =
                    Connection.init syms model.size

                newConns =
                    Dict.insert id c model.connections
            in
                ( { model | connections = newConns }, Cmd.map (ModifyConnection id) cx )

        Modify id msg ->
            noFx model

        ModifyConnection id msg ->
            noFx model


viewSymbol : ( Int, Symbol.Model ) -> Svg Msg
viewSymbol ( id, sym ) =
    let
        pos =
            sym.pos
    in
        Svg.g []
            [ App.map (Modify id) (Symbol.view sym)
            , Svg.text' [ SA.x <| sx pos, SA.y <| sy pos ] [ Svg.text <| toString id ]
            ]


viewConnection : ( Int, Connection.Model ) -> Svg Msg
viewConnection ( id, con ) =
    App.map (ModifyConnection id) (Connection.view con)


view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        Svg.svg
            [ SA.width sw
            , SA.height sh
            , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
            ]
            ((List.map viewSymbol
                <| Dict.toList model.symbols
             )
                ++ (List.map viewConnection <| Dict.toList model.connections)
            )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ( Dict.values <| Dict.map (\k v -> Sub.map (Modify k) <| Symbol.subscriptions v ) model.symbols
        )
