module BasicElmDepsVisualization exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Diagram
import Symbol
import Connection
import Layout exposing (LayoutNode, LayoutCell)
import BasicElmDeps
import Tuple2
import Graph exposing (Graph)


type alias Model a =
    { layout : Layout.Model a
    , diagram : Diagram.Model
    }


type Msg
    = DiagramMsg Diagram.Msg


init : Graph String () -> ( Model a, Cmd Msg )
init depGraph =
    let
        layout =
            Layout.init depGraph

        nodes = Layout.getNodes layout

        ( syms, symsFx) = createSymbols nodes

        conns = List.map (createConnection syms layout) depGraph.edges

        ( dg, dgFx ) =
            Diagram.init syms conns
    in
        { layout = layout, diagram = dg } ! [ Cmd.map DiagramMsg dgFx ]



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


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )


diagram : Model a -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


view : Model a -> Svg Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div []
            [ diagram model
            ]
        , Html.div
            [ HA.style
                [ (,) "clear" "both"
                , (,) "position" "relative"
                ]
            ]
            [ Html.text <| toString model ]
        ]


main : Program Never
main =
    App.program
        { init =
            init BasicElmDeps.init
            -- DependencyGraph.uut
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
