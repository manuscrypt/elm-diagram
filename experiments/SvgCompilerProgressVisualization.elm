module SvgCompilerProgressVisualization exposing (nothing)

import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html.App as Html
import Time exposing (Time, second)
import Extra.MathVector2 exposing (rotate)
import Extra.Svg as SvgUtils exposing (Stroke, bezierLineWithDirection, arrow)


nothing : String
nothing =
    "Nothing"


pfeilstroke : Stroke
pfeilstroke =
    Stroke Color.black 1


blackStroke : Stroke
blackStroke =
    SvgUtils.Stroke Color.black 2


pfeil : Vec2 -> Vec2 -> Color -> Svg a
pfeil targetPos unnormalizedDirection fillcolor =
    let
        fromdir =
            Math.Vector2.normalize (unnormalizedDirection)

        startPos =
            Math.Vector2.add targetPos (Math.Vector2.scale 15 fromdir)

        rightdir =
            (Math.Vector2.scale 5 (Extra.MathVector2.rotate fromdir 90))

        leftdir =
            Math.Vector2.scale -1 rightdir
    in
        let
            dstr =
                ("M "
                    ++ (SvgUtils.vecToStringX_Y targetPos)
                    ++ ", "
                    ++ (SvgUtils.vecToStringX_Y (Math.Vector2.add startPos rightdir))
                    ++ ", "
                    ++ (SvgUtils.vecToStringX_Y (Math.Vector2.add startPos leftdir))
                    ++ ", "
                    ++ (SvgUtils.vecToStringX_Y targetPos)
                )
        in
            Svg.path ([ SA.d dstr, SA.fill (colorToHex fillcolor) ] ++ (SvgUtils.strokeToSA pfeilstroke)) []


type alias ColorFunc =
    Int -> Color


type alias Node =
    { pos : Vec2
    , colorfunc : ColorFunc
    }


createNode : Float -> Float -> ColorFunc -> Node
createNode posX posY colorfunc =
    Node (vec2 posX posY) colorfunc


alwaysWhite : Int -> Color
alwaysWhite t =
    Color.white


nodes : List Node
nodes =
    [ (createNode 70
        50
        (\t ->
            if t < 1 then
                Color.white
            else if t < 2 then
                Color.blue
            else
                Color.green
        )
      )
    , (createNode 210
        50
        (\t ->
            if t < 1 then
                Color.white
            else if t < 4 then
                Color.blue
            else
                Color.green
        )
      )
    , (createNode 350
        50
        (\t ->
            if t < 1 then
                Color.white
            else if t < 3 then
                Color.blue
            else
                Color.green
        )
      )
    , (createNode 130
        150
        (\t ->
            if t < 4 then
                Color.white
            else if t < 5 then
                Color.blue
            else
                Color.green
        )
      )
    , (createNode 70
        250
        (\t ->
            if t < 5 then
                Color.white
            else if t < 7 then
                Color.blue
            else
                Color.red
        )
      )
    , (createNode 270
        250
        (\t ->
            if t < 5 then
                Color.white
            else if t < 6 then
                Color.blue
            else
                Color.green
        )
      )
    , (createNode 50 350 alwaysWhite)
    , (createNode 190 350 alwaysWhite)
    , (createNode 330
        350
        (\t ->
            if ((t % 2) == 0) then
                Color.lightGrey
            else
                Color.gray
        )
      )
    ]


nodesToCircles : { a | secsSinseStart : Int } -> List (Html b)
nodesToCircles model =
    List.map (\node -> SvgUtils.circle node.pos 20 blackStroke (node.colorfunc model.secsSinseStart)) nodes


template : Html a
template =
    Html.img
        [ HA.style [ (,) "position" "absolute", (,) "top" "-10px", (,) "left" "-10px", (,) "border" "1px solid black", (,) "opacity" "0.1", (,) "z-index" "2" ]
        , HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif"
        ]
        []


template2 : Html a
template2 =
    Html.img
        [ HA.style [ (,) "position" "absolute", (,) "top" "-10px", (,) "left" "450px", (,) "border" "1px solid black", (,) "z-index" "2" ]
        , HA.src "https://raw.githubusercontent.com/elm-lang/projects/master/compiler-progress-visualization/mock.gif"
        ]
        []


svgDiag : { a | secsSinseStart : Int } -> Html b
svgDiag model =
    (svg
        [ version "1.1"
        , x "0"
        , y "0"
        , SA.width "800"
        , SA.height "600"
        , viewBox "0 0 800 600"
        ]
        ((nodesToCircles model)
            --++ directionPoints
            ++
                [ (SvgUtils.bezierLineWithDirection (vec2 70 67.5)
                    (vec2 0 40)
                    (vec2 -20 -10)
                    (vec2 115 135)
                    (if (model.secsSinseStart > 5) then
                        SvgUtils.blueThinStroke
                     else
                        blackStroke
                    )
                  )
                , (pfeil (vec2 115 135) (vec2 -17 -10) Color.red)
                , (SvgUtils.bezierLineWithDirection (vec2 210 67.5) (vec2 0 40) (vec2 20 -30) (vec2 145 135) blackStroke)
                , (pfeil (vec2 145 135)
                    (vec2 9 -10)
                    (if (model.secsSinseStart < 3) then
                        Color.lightOrange
                     else
                        Color.black
                    )
                  )
                ]
        )
    )


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { actTime : Time
    , startTime : Time
    , secsSinseStart : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { actTime = 0
      , startTime = 0
      , secsSinseStart = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Tick newTime ->
            let
                newStartTime =
                    if model.startTime == 0 then
                        newTime
                    else
                        model.startTime

                timeSinceStart =
                    newTime - newStartTime

                newSecsSinseStart =
                    ((round ((Time.inMinutes timeSinceStart) * 100)) % 17)
            in
                ( { model
                    | actTime = newTime
                    , startTime = newStartTime
                    , secsSinseStart = newSecsSinseStart
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.millisecond Tick



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ (svgDiag model)
        , template
        , template2
        , text ((toString model.actTime) ++ "-" ++ (toString model.startTime) ++ "=" ++ (toString model.secsSinseStart))
        ]
