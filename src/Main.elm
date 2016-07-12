module Main exposing (..)

import Visuals.Diagram as Diagram
import Html.App as App
import Html exposing (Html)
import Html.Attributes as HA

import Math.Vector2 exposing (vec2)

type alias Model
  = { diagram : Diagram.Model }

type Msg
    = DiagramMsg Diagram.Msg

main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

init: (Model, Cmd Msg)
init =
    let (dg, cmd) = Diagram.init (vec2 800 800)
    in { diagram = dg } ! []

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DiagramMsg msg ->
      let ( dg, dgCmd ) = Diagram.init (vec2 500 500)
      in { model | diagram = dg } ! [ Cmd.map DiagramMsg dgCmd ]

view : Model -> Html Msg
view model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            , (,) "border" "1px solid violet"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]
