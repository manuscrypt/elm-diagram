module BasicElmDepsVisualization exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Svg exposing (Svg)
import Extra.Cmd exposing (noFx, updateOne, updateMany)
import Diagram exposing (..)
import Symbol
import CompilationUnit exposing (..)
import BasicElmDeps

rootCU : List CompilationUnit.Model
rootCU = 
    [ fst BasicElmDeps.init         
    ]

type alias Model =
    { diagram : Diagram.Model
    }


type Msg
    = NoOp
    | DiagramMsg Diagram.Msg


createDiagram : List CompilationUnit.Model -> Diagram.Model
createDiagram roots =
    let d = ( fst Diagram.init )
    --in Diagram.update ( Diagram.AddNode Color.white (vec2 20 20) (vec2 100 100 ) )
    in d  

init : ( Model, Cmd Msg )
init =
    ( { diagram = ( createDiagram rootCU ) }, Cmd.none )
{--
    let
        msgs =
            List.map (\s -> DiagramMsg <| AddNode Color.white (vec2 20 20) <| toVec s) sample

        msgs' =
            List.map (\a -> DiagramMsg <| Connect a) sampleCons3

        ( d, dx ) =
            Diagram.init

        m0 =
            ( { diagram = d
              }
            , Cmd.batch [ Cmd.map DiagramMsg dx ]
            )
    in
        updateMany (msgs ++ msgs') update m0
        --}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            noFx model

        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )

diagram : Model -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


view : Model -> Svg Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div []
            [ diagram model
            ]
        , Html.div [ HA.style [ (,) "clear" "both", (,) "position" "relative" ] ] [ Html.text <| toString model ]
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
