module Dynamic.Sample1 exposing (..)

import VirtualDom exposing (Node)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Html exposing (Html)
import Html.App as Html
import Time exposing (Time, second)
import Layouts.Rules as DynamicLayout
import Dynamic.SvgVisualization as SvgVisualization


--  SAMPLE


type alias SampleNode =
    { name : String
    }


sampleNode0 : { name : String }
sampleNode0 =
    { name = "n0" }


sampleNode1 : { name : String }
sampleNode1 =
    { name = "n1" }


sampleNode2 : { name : String }
sampleNode2 =
    { name = "n2" }


sampleNode3 : { name : String }
sampleNode3 =
    { name = "n3" }


sampleNode4 : { name : String }
sampleNode4 =
    { name = "n4" }


sampleNode5 : { name : String }
sampleNode5 =
    { name = "n5" }


sampleNode6 : { name : String }
sampleNode6 =
    { name = "n6" }


sampleNode7 : { name : String }
sampleNode7 =
    { name = "n7" }


sampleNode8 : { name : String }
sampleNode8 =
    { name = "n8" }


sampleNodes : List { name : String }
sampleNodes =
    [ sampleNode0
    , sampleNode1
    , sampleNode2
    , sampleNode3
    , sampleNode4
    , sampleNode5
    , sampleNode6
    , sampleNode7
    , sampleNode8
    ]
        ++ List.map (\nr -> { name = "x" ++ (toString nr) }) [10..15]


sampleConnections : List ( { name : String }, { name : String } )
sampleConnections =
    [ ( sampleNode0, sampleNode3 )
    , ( sampleNode1, sampleNode3 )
    , ( sampleNode2, sampleNode5 )
    , ( sampleNode3, sampleNode4 )
    , ( sampleNode3, sampleNode5 )
    , ( sampleNode4, sampleNode6 )
    , ( sampleNode5, sampleNode7 )
    , ( sampleNode5, sampleNode8 )
    , ( { name = "x15" }, { name = "x10" } )
    , ( { name = "x10" }, { name = "x11" } )
    , ( { name = "x11" }, { name = "x12" } )
    , ( { name = "x12" }, { name = "x13" } )
    , ( { name = "x13" }, { name = "x14" } )
    , ( { name = "x14" }, { name = "x15" } )
    ]


sampleLayout : DynamicLayout.Model { name : String }
sampleLayout =
    DynamicLayout.addForEachRule (DynamicLayout.noIntersection 100)
        <| DynamicLayout.addForOneRule (DynamicLayout.snapToGrid 100)
        <| DynamicLayout.addForTwoRule sampleNode6 (DynamicLayout.hasSameY 0.05) sampleNode7
        <| DynamicLayout.addForTwoRule sampleNode6 (DynamicLayout.hasSameY 0.05) sampleNode8
        <| DynamicLayout.addForTwoRule sampleNode7 (DynamicLayout.hasSameY 0.05) sampleNode8
        <| makeDepsSameY
        <| (List.foldl makeConn
                (List.foldl DynamicLayout.addNode DynamicLayout.init sampleNodes)
                sampleConnections
           )


makeConn : ( SampleNode, SampleNode ) -> DynamicLayout.Model SampleNode -> DynamicLayout.Model SampleNode
makeConn ( nodeA, nodeB ) layout =
    DynamicLayout.addForTwoRule nodeA (DynamicLayout.isAbove 50) nodeB
        <| DynamicLayout.addForTwoRule nodeA (DynamicLayout.hasSameX 0.05) nodeB layout


makeDepsSameYforAB : ( SampleNode, SampleNode ) -> DynamicLayout.Model SampleNode -> DynamicLayout.Model SampleNode
makeDepsSameYforAB ( a, b ) layout =
    if (a == b) then
        layout
    else
        DynamicLayout.addForTwoRule a (DynamicLayout.hasSameY 0.05) b layout


zip : List a -> List a -> List ( a, a )
zip la lb =
    List.concat <| List.map (\aa -> List.map (\bb -> ( aa, bb )) lb) la


makeDepsSameYfor : SampleNode -> DynamicLayout.Model SampleNode -> DynamicLayout.Model SampleNode
makeDepsSameYfor parent layout =
    let
        relcons =
            List.filter (\( a, b ) -> (b == parent)) sampleConnections

        relchs =
            List.map (\( a, b ) -> a) relcons

        zipped =
            zip relchs relchs

        -- List.map ( \a -> List.map ( \b -> ( a, b ) ) relchs ) relchs
    in
        List.foldr makeDepsSameYforAB layout zipped


makeDepsSameY : DynamicLayout.Model SampleNode -> DynamicLayout.Model SampleNode
makeDepsSameY layout =
    List.foldr makeDepsSameYfor layout sampleNodes



-- MODEL


type alias Model =
    { nodes : List SampleNode
    , layout : DynamicLayout.Model SampleNode
    , actTime : Time
    , startTime : Time
    , secsSinseStart : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { nodes = sampleNodes
      , layout = sampleLayout
      , actTime = 0
      , startTime = 0
      , secsSinseStart = 0
      }
    , Cmd.none
    )


addNode :
    a
    -> { b | nodes : List a, layout : DynamicLayout.Model a }
    -> { b | layout : DynamicLayout.Model a, nodes : List a }
addNode node model =
    { model
        | nodes = model.nodes ++ [ node ]
        , layout = DynamicLayout.addNode node model.layout
    }


sample : ( Model, Cmd Msg )
sample =
    ( (fst init)
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
                    , layout = DynamicLayout.animate model.layout
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (17 * Time.millisecond) Tick



-- VIEW


viewboxMargin : number
viewboxMargin =
    50


viewSvgNodes : Model -> List (VirtualDom.Node a)
viewSvgNodes model =
    (SvgVisualization.grid (DynamicLayout.viewbox viewboxMargin model.layout))
        ++ (List.concat
                <| List.map
                    (\( nodeA, nodeB ) ->
                        SvgVisualization.connection (DynamicLayout.positionOfNode nodeA model.layout)
                            (DynamicLayout.positionOfNode nodeB model.layout)
                    )
                    sampleConnections
           )
        ++ (List.concat
                <| List.map
                    (\node ->
                        SvgVisualization.node (DynamicLayout.positionOfNode node model.layout)
                            node.name
                    )
                    model.nodes
           )


view : Model -> Html Msg
view model =
    Html.div []
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , SA.width "600"
            , SA.height "500"
            , viewBox (DynamicLayout.viewboxAsString viewboxMargin model.layout)
            ]
            ((viewSvgNodes model))
        , text ((toString model.actTime) ++ "-" ++ (toString model.startTime) ++ "=" ++ (toString model.secsSinseStart))
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
