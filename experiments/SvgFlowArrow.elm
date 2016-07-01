module SvgFlowArrow exposing (toSvg)

import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onInput)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import String
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Html.App as Html
import Time exposing (Time, second)
import Extra.Svg as SvgUtils exposing (..)
import Extra.MathVector2 exposing (rotate)


-- MODEL


type alias Model =
    { actTime : Time
    , startTime : Time
    , secsSinseStart : Int
    , startPos : Vec2
    , endPos : Vec2
    , startToEndDirection : Vec2
    , toRightDirection : Vec2
    , toRight : Vec2
    , width : Float
    , fillColor : Color
    , outlineStroke : SvgUtils.Stroke
    }


recalc : Model -> Model
recalc model =
    let
        newStartToEndDirection =
            Math.Vector2.normalize (Math.Vector2.sub model.endPos model.startPos)

        newToRightDirection =
            Extra.MathVector2.rotate newStartToEndDirection 90

        newToRight =
            Math.Vector2.scale (model.width / 2) newToRightDirection
    in
        { model
            | startToEndDirection = newStartToEndDirection
            , toRightDirection = newToRightDirection
            , toRight = newToRight
        }


init : ( Model, Cmd Msg )
init =
    ( recalc
        { actTime = 0
        , startTime = 0
        , secsSinseStart = 0
        , startPos = vec2 50 200
        , endPos = vec2 450 300
        , startToEndDirection = vec2 0 0
        , toRightDirection = vec2 0 0
        , toRight = vec2 0 0
        , width = 50
        , fillColor = Color.blue
        , outlineStroke = SvgUtils.Stroke Color.black 5
        }
    , Cmd.none
    )


toSvg :
    { a
        | endPos : Vec2
        , fillColor : Color
        , outlineStroke : Stroke
        , startPos : Vec2
        , toRight : Vec2
    }
    -> Html b
toSvg model =
    let
        dstr =
            ("M "
                ++ (SvgUtils.vecToStringX_Y model.startPos)
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y (Math.Vector2.add model.startPos model.toRight))
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y (Math.Vector2.add model.endPos model.toRight))
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y model.endPos)
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y (Math.Vector2.sub model.endPos model.toRight))
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y (Math.Vector2.sub model.startPos model.toRight))
                ++ ", "
                ++ (SvgUtils.vecToStringX_Y model.startPos)
            )
    in
        svg [ version "1.1", SA.width "800", SA.height "600" ]
            [ Svg.path ([ SA.d dstr, SA.fill (colorToHex model.fillColor) ] ++ (SvgUtils.strokeToSA model.outlineStroke)) []
            ]


inputToColor : Color -> String -> Color
inputToColor oldColor newColorString =
    (Maybe.withDefault oldColor (Color.Convert.hexToColor newColorString))


inputToFloat : Float -> String -> Float
inputToFloat oldValue newValueString =
    (Result.withDefault oldValue (String.toFloat newValueString))



--inputToVec2 : Vec2 -> String -> Vec2
--inputToVec2 oldValue newValueString = ( Maybe.withDefault oldColor ( Color.Convert.hexToColor newColorString ) )
-- UPDATE


type Msg
    = Tick Time
    | SetWidthString String
    | SetOutlineStrokeWidthString String
    | SetOutlineStrokeColorString String
    | SetFillColorString String


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
                    ((round ((Time.inMinutes timeSinceStart) * 100)) % 360)

                newStartPos =
                    vec2 350 350

                newEndPos =
                    Math.Vector2.add newStartPos (Extra.MathVector2.rotate (vec2 300 0) (timeSinceStart * 0.052))
            in
                ( recalc
                    { model
                        | actTime = newTime
                        , startTime = newStartTime
                        , secsSinseStart = newSecsSinseStart
                        , startPos = newStartPos
                        , endPos = newEndPos
                    }
                , Cmd.none
                )

        SetWidthString widthString ->
            ( recalc { model | width = (inputToFloat model.width widthString) }, Cmd.none )

        SetOutlineStrokeWidthString widthString ->
            let
                oldStroke =
                    model.outlineStroke
            in
                ( { model | outlineStroke = { oldStroke | width = (inputToFloat oldStroke.width widthString) } }, Cmd.none )

        SetOutlineStrokeColorString colorString ->
            let
                oldStroke =
                    model.outlineStroke
            in
                ( { model | outlineStroke = { oldStroke | color = (inputToColor oldStroke.color colorString) } }, Cmd.none )

        SetFillColorString colorString ->
            ( { model | fillColor = (inputToColor model.fillColor colorString) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.millisecond Tick



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text "width: ", Html.input [ HA.value (toString model.width), onInput SetWidthString ] [] ]
        , Html.div [] [ Html.text "outlineStroke.width: ", Html.input [ HA.value (toString model.outlineStroke.width), onInput SetOutlineStrokeWidthString ] [] ]
        , Html.div [] [ Html.text "outlineStroke.color: ", Html.input [ HA.value (colorToHex model.outlineStroke.color), onInput SetOutlineStrokeColorString ] [] ]
        , Html.div [] [ Html.text "fillColor: ", Html.input [ HA.value (colorToHex model.fillColor), onInput SetFillColorString ] [] ]
        , Html.div [] [ (toSvg model) ]
        , text (" ----" ++ (toString model.actTime) ++ "-" ++ (toString model.startTime) ++ "=" ++ (toString model.secsSinseStart))
        ]



-- MAIN


main : Program Never
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
