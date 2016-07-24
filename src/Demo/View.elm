module Demo.View exposing (..)

import Html exposing (Html, div, text, h1, p, span, textarea, button)
import Html.Attributes as HA exposing (style, width, height, disabled, value)
import Html.Events as HE exposing (onClick, onInput)
import Html.App as App
import Graph exposing (Graph, Node)
import Random exposing (Seed, initialSeed)
import Date.Format as Date
import Visuals.Diagram.Diagram as Diagram
import Svg exposing (Svg)


---

import Demo.Model as Model exposing (Model, Msg(..))
import Visuals.Graph.AsList as GraphListView
import Visuals.Graph.AsTree as HtmlTree


view : Model -> Html Msg
view model =
    div
        [ style
            [ (,) "width" "920px"
            , (,) "margin" "auto"
            ]
        ]
        [ h1 [] [ text "compiler-progress-visualization" ]
        , div
            [ style
                [ (,) "display" "flex"
                ]
            ]
            [ left model
            , right model
            ]
          --, debug model
        ]


left : Model -> Html Msg
left model =
    Html.div [ style [ (,) "margin-right" "20px" ] ]
        [ info model
        , controls model
        , Html.div [ HA.style [ (,) "flex-flow" "row nowrap", (,) "display" "flex" ] ]
            [ Html.div [ HA.style [ (,) "flex" "auto" ] ] [ HtmlTree.viewGraph model.graph ]
            , Html.div [ HA.style [ (,) "flex" "auto" ] ] [ App.map GraphListViewMsg <| GraphListView.view model.listView ]
            ]
        ]


info : Model -> Html Msg
info model =
    let
        info =
            [ (,) "Now" (Maybe.withDefault "not set" <| Maybe.map (Date.format "%A, %B %d, %Y at %H:%M:%S") model.now)
            , (,) "Window-Size" (toString model.size)
            , (,) "Random #" (toString <| fst <| Random.step (Random.int 0 100) model.seed)
            , (,) "Message" model.errorMsg
            ]

        makeInfo ( label, text ) =
            div [ style [ (,) "flex" "auto" ] ]
                [ Html.b [] [ Html.text <| label ++ ": " ]
                , Html.text text
                ]
    in
        Html.div
            [ style
                [ (,) "display" "flex"
                , (,) "flex-flow" "column nowrap"
                ]
            ]
            <| List.map makeInfo info


controls : Model -> Html Msg
controls model =
    Html.div [ style [ (,) "margin-top" "20px" ] ]
        [ div [ style [ (,) "margin-top" "20px" ] ]
            [ span [] [ text "Paste ElmFile-graph or " ]
            , span [] [ button [ onClick FetchRemote, disabled <| not model.serverRunning ] [ text "Fetch current" ] ]
            , div
                [ style
                    [ (,) "margin-top" "3px"
                    ]
                ]
                [ textarea
                    [ onInput TextInputChanged
                    , value model.graphString
                    , style
                        [ (,) "width" "350px"
                        , (,) "height" "350px"
                        ]
                    ]
                    []
                ]
            ]
        ]


debug : Model -> Html Msg
debug model =
    Html.div []
        [ p [] [ text <| Graph.toString' model.graph ]
        , p [] [ text <| toString model.diagram ]
        ]


right : Model -> Html Msg
right model =
    Html.div []
        [ diagram model
        , animationButtons model
        ]


diagram : Model -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            , (,) "border" "1px solid violet"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


animationButtons : Model -> Html Msg
animationButtons model =
    span []
        [ text "Animation: "
        , button [ onClick Start, disabled <| (model.running || model.graph == Graph.empty) ] [ text "Start" ]
        , button [ onClick Stop, disabled <| not model.running ] [ text "Stop" ]
        , text (" dt: " ++ toString (model.lastDt))
        ]
