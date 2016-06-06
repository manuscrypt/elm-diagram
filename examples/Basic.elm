import Html.App as App
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
  let x = { diagram = fst Diagram.init }
      y = fst <| update (AddBall 110 120) x 
      z = fst <| update (AddBall 130 320) y
  in noFx z

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBall x y ->
          let (d,fx) = Diagram.update (Add Symbol.Circle Color.white (vec2 20 20 ) (vec2 x y)) model.diagram
          in noFx { model | diagram = d }
        DiagramMsg msg -> 
          noFx model


view : Model -> Svg Msg
view model =
  App.map DiagramMsg <| Diagram.view model.diagram

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_->Sub.none)
    }
