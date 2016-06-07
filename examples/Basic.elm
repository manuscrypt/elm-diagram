import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Svg exposing (Svg)

import Util exposing ( noFx)
import Diagram exposing (..)
import Symbol exposing (..)

type alias Model = 
  { diagram: Diagram.Model
  }

type Msg 
  = AddBall Float Float
  | DiagramMsg Diagram.Msg


init : ( Model, Cmd Msg )
init =
  let a = { diagram = fst Diagram.init }
      b = fst <| update (AddBall 70 50 ) a
      c = fst <| update (AddBall 210 50 ) b
      d = fst <| update (AddBall 350 50 ) c
      e = fst <| update (AddBall 130 150 ) d
      f = fst <| update (AddBall 70 250 ) e
      g = fst <| update (AddBall 270 250 ) f
      h = fst <| update (AddBall 50 350 ) g
      i = fst <| update (AddBall 190 350 ) h
      k = fst <| update (AddBall 330 350 ) i
      l = fst <| update (DiagramMsg (Connect 0 1)) k
  in noFx { l | diagram = fst <| Diagram.update (Diagram.Connect 1 3) l.diagram }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBall x y ->
          let (d,fx) = Diagram.update (Add Symbol.Circle Color.white (vec2 20 20 ) (vec2 x y)) model.diagram
          in noFx { model | diagram = d }
        DiagramMsg msg -> 
          noFx model

diagram: Model->Svg Msg
diagram model = 
  Html.div [ HA.style [ (,) "z-index" "1"
                      , (,) "opacity" "1"
                      , (,) "position" "absolute"
                      ]
           ] [ App.map DiagramMsg <| Diagram.view model.diagram ]

template: Svg Msg
template = 
  Html.img [ HA.style [ (,) "position" "absolute"
             , (,) "top" "-10px"
             , (,) "left" "-10px"
             , (,) "border" "1px solid black"
             , (,) "opacity" "0.1"
             , (,) "z-index" "2"
              ]
           , HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif" 
           ] []

view : Model -> Svg Msg
view model =
    Html.div [ HA.style [] ]
      [ diagram model
      , template
      ]

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_->Sub.none)
    }