module Ball exposing (..)

import Animation exposing (Animation,run)
import Time exposing (Time)
import AnimationFrame exposing (..)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)

import Math.Vector2 exposing (Vec2, getX, getY)

import Symbol exposing (Shape)
import Util exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA

type alias Model = 
  { animation : Animation Float
  , shape : Shape
  , color : Color
  , size : Vec2
  , pos : Vec2
  }

type Msg 
  = Animate Time

init : Color -> Vec2 -> Vec2 -> ( Model, Cmd Msg )
init color size pos =
    { animation = zeroState
    , shape = Symbol.Circle
    , color = color
    , size = size
    , pos = pos
    } ! []


zeroState : Animation Float
zeroState =
    Animation.immediately 0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      Animate dt ->
          noFx { model | animation = Animation.run dt model.animation }


view : Model -> Svg Msg
view model =
  Svg.circle (attrs <| circle model) []


circle : Model -> List ( String -> Svg.Attribute b, String )
circle model =
    [ ( SA.cx, "0" )
    , ( SA.cy, "0" )
    , ( SA.r, toString <| getX model.size )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    ]

attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


subscriptions : Model -> Sub Msg
subscriptions model =
  if Animation.isDone model.animation then
    Sub.none
  else
    AnimationFrame.diffs Animate