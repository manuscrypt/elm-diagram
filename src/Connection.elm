module Connection exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA

import Util exposing (noFx)
import Color exposing (Color)
import Symbol
import Spline exposing (splines)

type alias Model =
    { symbols: List (Symbol.Model)
    , width : Int
    , stroke : Color
    }


type Msg
    = NoOp  

splineStyle: String
splineStyle =  "fill:transparent;stroke:red;stroke-width:2"

init : List Symbol.Model -> ( Model, Cmd Msg )
init symbols =
    noFx <| Model symbols 3 Color.black

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp  ->
            noFx model

view : Model -> Svg Msg
view model =
    let paths = List.map (\p -> Svg.path [SA.d p, SA.style splineStyle][] ) <| splines (List.map .pos model.symbols)
    in Svg.g [] paths