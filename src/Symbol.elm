module Symbol exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Util exposing (noFx)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Time exposing (Time)
import Animation exposing (Animation)
import AnimationFrame


type Shape
    = Circle
    | Rect


type alias Model =
    { shape : Shape
    , color : Color
    , size : Vec2
    , pos : Vec2
    , animation : Animation Color
    }


type Msg
    = Move Vec2
    | Resize Vec2
    | Animate Time.Time


animationStates =
    [ Color.white, Color.grey, Color.red, Color.green ]


init : Shape -> Color -> Vec2 -> Vec2 -> ( Model, Cmd Msg )
init shape color size pos =
    noFx <| Model shape color size pos animation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move pos ->
            noFx { model | pos = pos }

        Animate dt ->
            noFx
                { model
                    | animation = Animation.run dt model.animation
                    , color = Animation.sample model.animation
                }

        Resize size ->
            noFx { model | size = size }


view : Model -> Svg Msg
view model =
    let
        content =
            toSvg model
    in
        Svg.g [ SA.transform <| translate model.pos ] [ content ]


toSvg : Model -> Svg Msg
toSvg model =
    case model.shape of
        Circle ->
            Svg.circle (attrs <| circle model) []

        Rect ->
            Svg.rect (attrs <| rect model) []


translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"


attrs : List ( a -> b, a ) -> List b
attrs list =
    List.map (\( k, v ) -> k v) list


circle : Model -> List ( String -> Svg.Attribute b, String )
circle model =
    [ ( SA.cx, "0" )
    , ( SA.cy, "0" )
    , ( SA.r, toString <| getX model.size )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    ]


rect : Model -> List ( String -> Svg.Attribute b, String )
rect model =
    [ ( SA.x, "0" )
    , ( SA.y, "0" )
    , ( SA.width, toString <| getX model.size )
    , ( SA.height, toString <| getY model.size )
    , ( SA.fill, colorToHex model.color )
    , ( SA.stroke, "black" )
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if Animation.isDone model.animation then
        Sub.none
    else
        AnimationFrame.diffs Animate


animation : Animation Color
animation =
    (2 * Time.second)
        |> Animation.interval
        |> Animation.map
            (\t ->
                if (t < 0.5) then
                    Color.green
                else
                    Color.red
            )



--    |> Animation.interval
--        |> Animation.map easing
