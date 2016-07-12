module Visuals.Visualization exposing (..)

import Graph exposing (Graph, Node, Edge, NodeId)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Visuals.Diagram as Diagram
import Model.CompilationUnit as CompilationUnit
import Model.ElmFile as ElmFile exposing (ElmFile)
import Html exposing (Html)
import Html.Attributes as HA
import Html.App as App
import Svg exposing (Svg)


type alias Model =
    { graph : Graph CompilationUnit.Model ()
    , diagram : Diagram.Model
    }


type Msg
    = DiagramMsg Diagram.Msg
    | Animate

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map DiagramMsg <| Diagram.subscriptions model.diagram
        ]


init : Graph ElmFile () -> Vec2 -> ( Model, Cmd Msg )
init elmFileGraph size =
    let
      graph =  CompilationUnit.fromElmFileGraph elmFileGraph
      diagramGraph = Diagram.diagramGraphFromCompilationUnitGraph graph
      ( dg, dgCmd ) =
            Diagram.init (vec2 500 500) diagramGraph
    in
        { graph = graph
        , diagram = Diagram.makeRadial <|  dg
        }
            ! [ Cmd.map DiagramMsg dgCmd
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

        Animate ->
            {-
            let
                ( model', cmd1 ) =
                    update (LayoutMsg Layout.Animate) model

                ( model'', cmd2 ) =
                    update (DiagramMsg (Diagram.SetPositions model'.layout.graph)) model'
            in
                model'' ! [ cmd1, cmd2 ]
              -}
              model ! []


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
