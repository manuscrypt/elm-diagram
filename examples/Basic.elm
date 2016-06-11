import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY,setX,setY)
import Svg exposing (Svg)

import Util exposing ( noFx)
import Diagram exposing (..)
import Symbol exposing (..)
import Tuple2 exposing (..)

type alias Model = 
  { diagram: Diagram.Model
  }

type Msg 
  = NoOp
  | AddBall Math.Vector2.Vec2
  | DiagramMsg Diagram.Msg

(=>) = (,)

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

sampleCons = 
  [ [0,1] 
  ,  [2,3,5]
  ]

toVec: (Float,Float) -> Math.Vector2.Vec2
toVec (x,y) = vec2 x y  

init : ( Model, Cmd Msg )
init =
    let msgs = List.map (\s -> AddBall <| toVec s) sample  
        msgs' = List.map (\a -> DiagramMsg <| Connect a) sampleCons
        (d,dx) = Diagram.init
        m0 = ({diagram = d}, Cmd.map DiagramMsg dx)
    in updateMany (msgs ++ msgs') m0

updateOne: Msg->( Model, Cmd Msg) -> ( Model, Cmd Msg )
updateOne msg (m,fx) = 
  let (m1,m1x) = update msg m
  in m1 ! [fx, m1x]

updateMany : List Msg -> ( Model, Cmd Msg) -> ( Model, Cmd Msg )
updateMany msgs modelCmd = List.foldl updateOne modelCmd msgs

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> noFx model
          
        AddBall vec -> 
          let msg = Diagram.Add Symbol.Circle Color.white (vec2 20 20) vec
          in update (DiagramMsg msg) model
        
        DiagramMsg msg -> 
          let (d,fx) = Diagram.update msg model.diagram
          in ({ model | diagram = d }, Cmd.map DiagramMsg fx)

diagram: Model->Svg Msg
diagram model = 
  Html.div [ HA.style [ (,) "z-index" "1"
                      , (,) "opacity" "1"
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
           --, HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif" 
           ] []

view : Model -> Svg Msg
view model =
  Html.div[ bodyStyle ][
    Html.div []
      [ diagram model
      , template
      ]
      , Html.div [HA.style[(,) "clear" "both", (,) "position" "relative"]] [Html.text <| toString model]
  ]

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = (\_->Sub.none)
    }

bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]
