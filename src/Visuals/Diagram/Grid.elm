module Visuals.Diagram.Grid exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Extra.Svg exposing (Stroke, strokeToSA)
import VirtualDom
import Svg
import Svg.Attributes as SA
import Visuals.Defaults exposing (defaultGridStroke)


type alias GridDef =
    { xs : ( Float, Float )
    , ys : ( Float, Float )
    , stepSize : Float
    , stroke : Stroke
    }


type alias Model =
    { def : GridDef
    , lines : List GridLine
    }


type alias GridLine =
    { from : ( Float, Float )
    , to : ( Float, Float )
    , stroke : Stroke
    }


type Msg
    = Resize Vec2


defaultGridDef : Vec2 -> GridDef
defaultGridDef size =
    GridDef ( 0, getX size ) ( 0, getY size ) 25 defaultGridStroke


empty : Model
empty =
    init <| defaultGridDef (vec2 0 0)


init : GridDef -> Model
init def =
    { def = def
    , lines = makeLines def
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize size ->
            init <| defaultGridDef size


makeLines : GridDef -> List GridLine
makeLines def =
    let
        xLines =
            List.map (glx def.stroke def.xs) (steps def.xs def.stepSize)

        yLines =
            List.map (gly def.stroke def.ys) (steps def.ys def.stepSize)
    in
        xLines ++ yLines


steps : ( Float, Float ) -> Float -> List Float
steps ( min, max ) stepSize =
    let
        steps =
            round <| (max - min) / stepSize
    in
        List.map (\s -> min + toFloat s * stepSize) [0..(steps - 1)]


glx : Stroke -> ( Float, Float ) -> Float -> GridLine
glx stroke ( miny, maxy ) x =
    GridLine ( x, miny ) ( x, maxy ) stroke


gly : Stroke -> ( Float, Float ) -> Float -> GridLine
gly stroke ( minx, maxx ) y =
    GridLine ( minx, y ) ( maxx, y ) stroke


view : Model -> VirtualDom.Node a
view model =
    Svg.g [] (List.map viewLine model.lines)


viewLine : GridLine -> VirtualDom.Node a
viewLine { from, to, stroke } =
    let
        ps =
            List.map2 identity [ SA.x1, SA.y1, SA.x2, SA.y2 ]
                <| List.map toString [ fst from, snd from, fst to, snd to ]
    in
        Svg.line (ps ++ (strokeToSA stroke)) []
