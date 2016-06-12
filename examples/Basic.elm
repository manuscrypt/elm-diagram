module Main exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY)
import Svg exposing (Svg)
import Util exposing (noFx)
import Diagram exposing (..)
import Symbol exposing (..)
import Drag exposing (..)
import Event exposing (..)


type alias Model =
    { diagram : Diagram.Model
    , drag : Drag.Model Event.Model
    }


type Msg
    = NoOp
    | AddBall Math.Vector2.Vec2
    | DiagramMsg Diagram.Msg
    | DragMsg (Drag.Msg Event.Model)


(=>) =
    (,)


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


sample2 =
    [ 60 => 60
    , 220 => 300
    , 420 => 300
    , 700 => 240
    ]


sampleCons =
    [ --      [ 0, 3, 5, 8 ]
      --    , [ 1, 3, 4, 6 ]
      --    , [ 2, 5, 7 ]
      [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
    ]


sampleCons2 =
    [ [ 0, 3, 4, 6 ]
    , [ 1, 3, 5, 7 ]
    , [ 2, 5, 8 ]
    ]


toVec : ( Float, Float ) -> Math.Vector2.Vec2
toVec ( x, y ) =
    vec2 x y


init : ( Model, Cmd Msg )
init =
    let
        msgs =
            List.map (\s -> AddBall <| toVec s) sample

        msgs' =
            List.map (\a -> DiagramMsg <| Connect a) sampleCons2

        ( d, dx ) =
            Diagram.init

        ( drag, dragCmd ) =
            Drag.init

        m0 =
            ( { diagram = d
              , drag = drag
              }
            , Cmd.batch [ Cmd.map DiagramMsg dx, Cmd.map DragMsg dragCmd ]
            )
    in
        updateMany (msgs ++ msgs') m0


updateMany : List Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateMany msgs modelCmd =
    List.foldl updateOne modelCmd msgs


updateOne : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateOne msg ( model, effect ) =
    let
        ( next, nextEffect ) =
            update msg model
    in
        next ! [ effect, nextEffect ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            noFx model

        AddBall vec ->
            let
                msg =
                    Diagram.Add Symbol.Circle Color.white (vec2 20 20) vec
            in
                update (DiagramMsg msg) model

        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )

        DragMsg msg ->
            let
                ( drag', cmd', event ) =
                    Drag.update msg model.drag

                model' =
                    { model | drag = drag' }

                ( model'', cmd'' ) =
                    onDragEvent event model'
            in
                ( model'', Cmd.batch [ Cmd.map DragMsg cmd', cmd'' ] )


onDragEvent : Drag.Event Event.Model -> Model -> ( Model, Cmd Msg )
onDragEvent event model =
    case event of
        Drag.OnMove position event ->
            let
                offset' =
                    Drag.calculateOffsetWithinBounds model.drag event.offset (round <| getY model.diagram.size) event.amount

                event' =
                    { event | offset = offset' }
            in
                model ! []

        _ ->
            model ! []


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
            , (,) "left" "-10px"
            , (,) "border" "1px solid black"
            , (,) "opacity" "0.1"
            , (,) "z-index" "2"
            ]
          --, HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif"
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
        , subscriptions = subscriptions
        }


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map DragMsg <| Drag.subscriptions model.drag
        , Sub.map DiagramMsg <| Diagram.subscriptions model.diagram
        ]
