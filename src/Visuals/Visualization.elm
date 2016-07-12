module Visuals.Visualization exposing (..)

import Graph exposing (Graph, Node, Edge, NodeId)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Visuals.Diagram as Diagram
import Visuals.Symbol as Symbol
import Visuals.Connection as Connection
import Model.CompilationUnit as CompilationUnit
import Model.Layout as Layout exposing (PosAndLabel)
import Model.ElmFile as ElmFile exposing (ElmFile)
import Html exposing (Html)
import Html.Attributes as HA
import Html.App as App
import Svg exposing (Svg)
import Random exposing (initialSeed)


type alias Model =
    { graph : Graph CompilationUnit.Model ()
    , layout : Layout.Model
    , diagram : Diagram.Model
    }


type Msg
    = DiagramMsg Diagram.Msg
    | LayoutMsg Layout.Msg
    | Animate


init : Graph ElmFile () -> Vec2 -> ( Model, Cmd Msg )
init elmFileGraph size =
    let
        ( lay, layCmd ) =
            Layout.init elmFileGraph size (Random.initialSeed 48)

        ( dg, dgCmd ) =
            Diagram.init (vec2 500 500) lay.graph
    in
        { graph = CompilationUnit.fromElmFileGraph elmFileGraph
        , layout = lay
        , diagram = dg
        }
            ! [ Cmd.map DiagramMsg dgCmd
              , Cmd.map LayoutMsg layCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiagramMsg dMsg ->
            let
                ( dg, dgCmd ) =
                    Diagram.update dMsg model.diagram
            in
                { model | diagram = dg } ! [ Cmd.map DiagramMsg dgCmd ]

        LayoutMsg lMsg ->
            let
                ( l, lCmd ) =
                    Layout.update lMsg model.layout
            in
                { model | layout = l } ! [ Cmd.map LayoutMsg lCmd ]

        Animate ->
            let
                ( model', cmd1 ) =
                    update (LayoutMsg Layout.Animate) model

                ( model'', cmd2 ) =
                    update (DiagramMsg (Diagram.SetPositions model'.layout.graph)) model'
            in
                model'' ! [ cmd1, cmd2 ]


view : Model -> Html Msg
view model =
    Html.div []
        [ diagram model
        ]


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
