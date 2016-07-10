module BasicDiagram exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Svg exposing (Svg)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub, fromTuple)
import Extra.Cmd exposing (noFx, updateOne, updateMany)
import Visuals.Diagram as Diagram
import Visuals.Symbol as Symbol


type alias Model =
    { diagram : Diagram.Model
    }


type Msg
    = NoOp
    | DiagramMsg Diagram.Msg


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


sample : List ( number, number' )
sample =
    [ 70 => 50
    , 210 => 50
    , 350 => 50
    , 130 => 150
    , 70 => 250
    , 270 => 250
    , 50 => 350
    , 190 => 350
    , 330 => 350
    ]


sampleCons : List (List number)
sampleCons =
    [ [ 0, 3, 5, 8 ]
    , [ 1, 3, 4, 6 ]
    , [ 2, 5, 7 ]
    ]


init : ( Model, Cmd Msg )
init =
    let
        syms =
            fst <| List.unzip <| List.indexedMap (\i s -> Symbol.init i (toString i) <| fromTuple s) sample

        msgs' =
            List.map (\a -> DiagramMsg <| Diagram.Connect a) sampleCons

        ( d, dx ) =
            Diagram.init (vec2 500 500) syms []

        m0 =
            ( { diagram = d
              }
            , Cmd.batch [ Cmd.map DiagramMsg dx ]
            )
    in
        updateMany (msgs') update m0


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
    App.map DiagramMsg <| Diagram.view model.diagram


template : Svg Msg
template =
    Html.img
        [ HA.style
            [ (,) "position" "absolute"
            , (,) "top" "-10px"
            , (,) "left" "-9px"
            , (,) "border" "1px solid black"
            , (,) "opacity" "0.1"
            , (,) "z-index" "2"
            ]
        , HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif"
        ]
        []


view : Model -> Svg Msg
view model =
    Html.div
        [ HA.style
            [ (,) "border" "1px solid black"
            , (,) "background-color" "#EEEEEE"
            , (,) "width" ((toString <| round <| getX model.diagram.size) ++ "px")
            ]
        ]
        [ Html.div []
            [ diagram model
            , template
            ]
          --        , Html.div [ HA.style [ (,) "clear" "both", (,) "position" "relative" ] ] [ Html.text <| toString model ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
