module Basic exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Svg exposing (Svg)
import Extra.Cmd exposing (noFx, updateOne, updateMany)
import Diagram exposing (..)
import Symbol exposing (..)


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


sample2 : List ( number, number' )
sample2 =
    [ 60 => 60
    , 220 => 300
    , 420 => 300
    , 700 => 240
    ]


sampleCons : List (List number)
sampleCons =
    [ [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
    ]


sampleCons4 : List (List number)
sampleCons4 =
    [ [ 0, 1, 2, 8, 7, 6, 4, 3, 5 ]
    ]


sampleCons2 : List (List number)
sampleCons2 =
    [ [ 0, 3, 4, 6 ]
    , [ 1, 3, 5, 7 ]
    , [ 2, 5, 8 ]
    ]


sampleCons3 : List (List number)
sampleCons3 =
    [ [ 0, 3, 5, 8 ]
    , [ 1, 3, 4, 6 ]
    , [ 2, 5, 7 ]
    ]


toVec : ( Float, Float ) -> Math.Vector2.Vec2
toVec ( x, y ) =
    vec2 x y


init : ( Model, Cmd Msg )
init =
    let
        syms =
            fst <| List.unzip <| List.indexedMap (\i s -> Symbol.init i (toString i) Color.white (vec2 20 20) <| toVec s) sample

        msgs' =
            List.map (\a -> DiagramMsg <| Connect a) sampleCons3

        ( d, dx ) =
            Diagram.init syms []

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
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


template : Svg Msg
template =
    Html.img
        [ HA.style
            [ (,) "position" "absolute"
            , (,) "top" "-10px"
            , (,) "left" "35px"
            , (,) "border" "1px solid black"
            , (,) "opacity" "0.1"
            , (,) "z-index" "2"
            ]
        , HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif"
        ]
        []


view : Model -> Svg Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div []
            [ diagram model
            , template
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
