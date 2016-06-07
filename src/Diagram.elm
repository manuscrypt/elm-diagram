module Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html
import Html.Attributes as HA exposing (..)
import Html.App as App
import Color exposing (Color)
import Dict exposing (insert)
import Symbol
import Connection
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)


type alias Model =
    { size : Vec2
    , gridSize : Vec2
    , symbols : Dict.Dict Int Symbol.Model
    , connections : Dict.Dict Int Connection.Model
    }

type Msg
    = Add Symbol.Shape Color Vec2 Vec2
    | Connect Int Int
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

        Connect from to ->
            let a = Dict.get from model.symbols |> Maybe.map .pos |> Maybe.withDefault (vec2 0 0)
                b = Dict.get to model.symbols |> Maybe.map .pos |> Maybe.withDefault (vec2 0 0)
                newConns = Dict.insert (Dict.size model.connections) (Connection.Model a b 3 Color.black) model.connections         
            in noFx { model | connections = newConns }

        Modify id msg ->
            noFx model
        ModifyConnection id msg ->
            noFx model


viewSymbol : ( Int, Symbol.Model ) -> Svg Msg
viewSymbol ( id, sym ) =
    App.map (Modify id) (Symbol.view sym)

viewConnection : ( Int, Connection.Model ) -> Svg Msg
viewConnection ( id, con ) =
    App.map (ModifyConnection id) (Connection.view con)


view : Model -> Svg Msg
view model =
    let
        ( sw, sh ) =
            ( toString <| getX model.size, toString <| getY model.size )
    in
        Html.div [ bodyStyle ]
            [ Svg.svg
                [ SA.width sw
                , SA.height sh
                , SA.viewBox <| "0 0 " ++ sw ++ " " ++ sh
                ]
                (( List.map viewSymbol
                <| Dict.toList model.symbols) ++ 
                ( List.map viewConnection <| Dict.toList model.connections)) 
            ]


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]
