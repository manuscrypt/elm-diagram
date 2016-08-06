module Extra.Svg exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import VirtualDom exposing (Node)
import String
import Color.Convert exposing (colorToHex)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Extra.Vec2 as Vec2 exposing (..)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY, fromTuple)


type alias Stroke =
    { color : Color
    , width : Float
    }


textCentered : Vec2 -> String -> String -> Svg c
textCentered pos style text =
    Svg.text'
        [ SA.x <| vecXtoString pos
        , SA.y <| vecXtoString pos
        , SA.textAnchor "middle"
        , SA.alignmentBaseline "middle"
        , SA.style style
        ]
        [ Svg.text text ]


arrow : Vec2 -> Vec2 -> Stroke -> Color -> Svg a
arrow targetPos unnormalizedDirection stroke fillcolor =
    let
        fromdir =
            Math.Vector2.normalize (unnormalizedDirection)

        startPos =
            Math.Vector2.add targetPos (Math.Vector2.scale 15 fromdir)

        rightdir =
            (Math.Vector2.scale 5 (Vec2.rotate fromdir 90))

        leftdir =
            Math.Vector2.scale -1 rightdir
    in
        let
            dstr =
                ("M "
                    ++ (vecToStringX_Y targetPos)
                    ++ ", "
                    ++ (vecToStringX_Y (Math.Vector2.add startPos rightdir))
                    ++ ", "
                    ++ (vecToStringX_Y (Math.Vector2.add startPos leftdir))
                    ++ ", "
                    ++ (vecToStringX_Y targetPos)
                )
        in
            Svg.path ([ SA.d dstr, SA.fill (colorToHex fillcolor) ] ++ (strokeToSA stroke)) []


circle : Vec2 -> Float -> Stroke -> Color -> VirtualDom.Node a
circle pos radius stroke fillcolor =
    Svg.circle
        ([ SA.cx (vecXtoString pos)
         , SA.cy (vecYtoString pos)
         , SA.r (toString radius)
         , SA.fill (colorToHex fillcolor)
         ]
            ++ (strokeToSA stroke)
        )
        []


bezierLineWithDirection : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Stroke -> VirtualDom.Node a
bezierLineWithDirection startPos startDir endDir endPos stroke =
    bezierLineWithControlPoints startPos (Math.Vector2.add startPos startDir) (Math.Vector2.add endPos endDir) endPos stroke


bezierLineWithControlPoints : Vec2 -> Vec2 -> Vec2 -> Vec2 -> Stroke -> VirtualDom.Node a
bezierLineWithControlPoints startPos startControlPos endControlPos endPos stroke =
    let
        dstr =
            ("M "
                ++ (vecToStringX_Y startPos)
                ++ " C "
                ++ (vecToStringX_Y startControlPos)
                ++ ", "
                ++ (vecToStringX_Y endControlPos)
                ++ ", "
                ++ (vecToStringX_Y endPos)
            )
    in
        Svg.path ([ SA.d dstr, SA.fill "transparent" ] ++ (strokeToSA stroke)) []


strokeToSA : Stroke -> List (VirtualDom.Property a)
strokeToSA stroke =
    [ SA.stroke (colorToHex stroke.color)
    , SA.strokeWidth (toString stroke.width)
    ]


vecXtoString : Vec2 -> String
vecXtoString v =
    toString (getX v)


vecYtoString : Vec2 -> String
vecYtoString v =
    toString (getY v)


vecToStringX_Y : Vec2 -> String
vecToStringX_Y v =
    (vecXtoString v) ++ " " ++ (vecYtoString v)



-- sample


redStroke : Stroke
redStroke =
    Stroke Color.red 1


redThinStroke : Stroke
redThinStroke =
    Stroke Color.red 0.3


blueThinStroke : Stroke
blueThinStroke =
    Stroke Color.blue 0.3


getXs : Vec2 -> String
getXs v =
    toString (getX v)


getYs : Vec2 -> String
getYs v =
    toString (getY v)


vecToStr : Vec2 -> String
vecToStr v =
    (getXs v) ++ " " ++ (getYs v)


vecToCircle : Vec2 -> VirtualDom.Node a
vecToCircle v =
    Svg.circle [ SA.cx (getXs v), SA.cy (getYs v), SA.r "1", SA.fill "transparent", SA.stroke "red" ] []


sampleToSvgPoints : List Vec2 -> String
sampleToSvgPoints sample =
    let
        head =
            vecToStr (Maybe.withDefault (vec2 0 0) (List.head sample))
    in
        let
            tails =
                String.concat (List.intersperse ", " (List.map vecToStr (Maybe.withDefault [] (List.tail sample))))

            --in let tails = List.tail sample
        in
            "M " ++ head ++ " C " ++ tails


sampleToSvg : List Vec2 -> List (VirtualDom.Node a)
sampleToSvg sample =
    (List.map vecToCircle sample)
        ++ [ (Svg.path [ SA.d (sampleToSvgPoints sample), SA.stroke "black", SA.fill "transparent" ] []) ]


path : String -> Vec2 -> ( Vec2, Vec2 ) -> ( Vec2, Vec2 ) -> Vec2 -> () -> Svg a
path st p1 ( cp1, x ) ( cp2, y ) p2 _ =
    let
        path =
            "M " ++ sp p1 ++ " C " ++ (String.join " " <| List.map sp [ cp1, cp2, p2 ])
    in
        Svg.path [ SA.style st, SA.d path ] []


toPath : String -> String -> Svg a
toPath style path =
    Svg.path [ SA.style style, SA.d path ] []


translate : Vec2 -> String
translate pos =
    "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"

translateTransform : Vec2 -> Svg.Attribute b
translateTransform pos =
    SA.transform <| "translate (" ++ (toString <| getX pos) ++ "," ++ (toString <| getY pos) ++ ")"




s : Vec2 -> String
s s =
    toString s


sxy : Vec2 -> String
sxy vec =
    (sx vec) ++ "," ++ (sy vec)


sp : Vec2 -> String
sp vec =
    (sx vec) ++ " " ++ (sy vec)


sx : Vec2 -> String
sx =
    toString << getX


sy : Vec2 -> String
sy =
    toString << getY


vecToSvgPos : Vec2 -> String
vecToSvgPos vec =
    (toString <| getX vec) ++ " " ++ (toString <| getY vec)


roundedRect : Vec2 -> Vec2 -> Float -> String -> Svg a
roundedRect pos size cornerRadius styleString =
    let
        ( px, py ) =
            (toStringTuple pos)

        ( sx, sy ) =
            (toStringTuple size)

        r =
            (toString cornerRadius)
    in
        Svg.rect
            [ x px
            , y py
            , SA.width sx
            , SA.height sy
            , rx r
            , ry r
            , SA.style styleString
            ]
            []



-- fromList : Vec2 -> List (Svg a) -> Html a
-- fromList size content =
--     let
--         viewRect =
--             [ -size.x / 2
--             , -size.y / 2
--             , size.x
--             , size.y
--             ]
--
--         --,
--     in
--         Svg.svg [ viewBox (join " " <| map toString viewRect) ] content
--
--


makeText : String -> Vec2 -> Float -> String -> Svg a
makeText str pos textHeight textStyle =
    text'
        [ SA.x (toString <| getX pos)
        , SA.y (toString <| getY pos)
        , SA.fontSize (toString textHeight)
        , SA.style textStyle
        ]
        [ Svg.text str ]



-- main : Html a
-- main =
--     let
--         pos =
--             vec2 150 150
--
--         size =
--             vec2 20 50
--
--         rr =
--             roundedRect pos size 5 "stroke: blue; stroke-width: 1"
--     in
--         Svg.svg [ viewBox "0 0 300 300" ] [ rr ]
--
--

--in

main : Node a
main =
    Svg.svg [ SA.version "1.1", SA.x "0", SA.y "0", SA.viewBox "0 0 323.141 322.95" ]
        [ (circle (vec2 100 10) 5 redThinStroke Color.yellow)
        , (circle (vec2 100 20) 15 redStroke Color.yellow)
        , (bezierLineWithDirection (vec2 50 40) (vec2 0 0) (vec2 0 0) (vec2 150 40) blueThinStroke)
        , (bezierLineWithDirection (vec2 50 50) (vec2 0 10) (vec2 0 -10) (vec2 150 50) blueThinStroke)
        , (bezierLineWithDirection (vec2 150 50) (vec2 0 10) (vec2 0 -10) (vec2 250 50) redThinStroke)
        , (bezierLineWithDirection (vec2 50 60) (vec2 0 20) (vec2 0 -20) (vec2 150 60) blueThinStroke)
        , (bezierLineWithDirection (vec2 150 60) (vec2 0 20) (vec2 0 -20) (vec2 250 60) redThinStroke)
        ]
