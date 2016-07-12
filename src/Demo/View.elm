module Demo.View exposing (..)

import Html exposing (Html, div, text, h1, p, span, textarea, button)
import Html.Attributes as HA exposing (style, width, height, disabled, value)
import Html.Events as HE exposing (onClick, onInput)
import Html.App as App
import Graph exposing (Graph, Node)
import Random exposing (Seed, initialSeed)
import Date.Format as Date
import Demo.Model as Model exposing (Model, Msg(..))
import Visuals.GraphListView as GraphListView
import Visuals.Visualization as Visualization


view : Model -> Html Msg
view model =
    div
        [ style
            [ (,) "width" "920px"
            , (,) "margin" "auto"
            ]
        ]
        [ h1 [] [ text "compiler-progress-visualization" ]
        , div [ style [ (,) "display" "flex" ] ] [ left model, right model ]
          --, debug model
        ]


left : Model -> Html Msg
left model =
    Html.div [ style [ (,) "margin-right" "20px" ] ]
        [ info model
        , controls model
        , App.map GraphListViewMsg <| GraphListView.view model.listView
        ]


info : Model -> Html Msg
info model =
    let
        info =
            [ (,) "Delta-T" (toString model.lastDt)
            , (,) "Now" (Maybe.withDefault "not set" <| Maybe.map (Date.format "%A, %B %d, %Y at %H:%M:%S") model.now)
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
        [ span []
            [ text "Animation: "
            , button [ onClick Start, disabled <| (model.running || model.graph == Graph.empty) ] [ text "Start" ]
            , button [ onClick Stop, disabled <| not model.running ] [ text "Stop" ]
            ]
        , div [ style [ (,) "margin-top" "20px" ] ]
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
        , p [] [ text <| toString model.visualization ]
        ]


right : Model -> Html Msg
right model =
    App.map VisualizationMsg <| Visualization.view model.visualization
