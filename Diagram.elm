module Diagram exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Html
import Html.Attributes as HA exposing (..)
import Html.App as App
import Dict exposing (insert)
import Symbol
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)


type alias Model =
    { size : Vec2
    , gridSize : Vec2
    , symbols :
        Dict.Dict Int Symbol.Model
        --, connections: Dict.Dict Int Connection.Model
    }


type Msg
    = Add Symbol.Model
    | Modify Int Symbol.Msg


init : ( Model, Cmd Msg )
init =
    noFx
        { size = vec2 800 600
        , gridSize = vec2 10 10
        , symbols =
            Dict.empty
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add sym ->
            let
                newSym =
                    insert (Dict.size model.symbols) sym model.symbols
            in
                noFx { model | symbols = newSym }

        Modify id msg ->
            noFx model


viewSymbol : ( Int, Symbol.Model ) -> Svg Msg
viewSymbol ( id, sym ) =
    App.map (Modify id) (Symbol.view sym)


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
                <| List.map viewSymbol
                <| Dict.toList model.symbols
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
        , (,) "margin-top" "100px"
        , (,) "background-color" "#EEEEEE"
        ]
