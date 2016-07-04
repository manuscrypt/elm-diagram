module DynamicLayoutSample2 exposing (..)

import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Html exposing (Html)
import Html.App as Html
import Time exposing (Time, second)
import DynamicLayout
import DynamicDiagram

-- MODEL

type alias Model =
  { diagram : DynamicDiagram.Model 
  }

init : ( Model, Cmd Msg )
init = ( { diagram =  DynamicDiagram.addImport { name = "x10" } { name = "x11" } 
                      <| DynamicDiagram.addImport { name = "x11" } { name = "x12" } 
                      <| DynamicDiagram.addImport { name = "x12" } { name = "x13" } 
                      <| DynamicDiagram.addImport { name = "x13" } { name = "x14" } 
                      <| DynamicDiagram.addImport { name = "x14" } { name = "x15" } 
                      <| DynamicDiagram.addImport { name = "x15" } { name = "x11" } 
                      <| DynamicDiagram.addRootNode { name = "x10" }                       
                      --<| DynamicDiagram.addImport { name = "x14" } { name = "x12" } 
                      --<| DynamicDiagram.addImport { name = "n1" } { name = "x14" } 
                      --<| DynamicDiagram.addImport { name = "n2" } { name = "x14" } 
--                      <| DynamicDiagram.addImport { name = "n5" } { name = "x14" } 
--                      <| DynamicDiagram.addImport { name = "n8" } { name = "x14" } 
--                      <| DynamicDiagram.addImport { name = "n8" } { name = "x10" } 
                      <| DynamicDiagram.addNode { name = "x11" }                       
                      <| DynamicDiagram.addNode { name = "x12" }                       
                      <| DynamicDiagram.addNode { name = "x13" }                       
                      <| DynamicDiagram.addNode { name = "x14" }                       
                      <| DynamicDiagram.addNode { name = "x15" }
                      <| DynamicDiagram.addRootNode { name = "n8" } 
                      <| DynamicDiagram.addImport { name = "n8" } { name = "n5" } 
                     <| DynamicDiagram.addNode { name = "n2" }
                      <| DynamicDiagram.addImport { name = "n7" } { name = "n5" } 
                      <| DynamicDiagram.addImport { name = "n5" } { name = "n3" } 
                      <| DynamicDiagram.addImport { name = "n5" } { name = "n2" } 
                     <| DynamicDiagram.addNode { name = "n5" }
                     <| DynamicDiagram.addRootNode { name = "n7" }
                      <| DynamicDiagram.addImport { name = "n3" } { name = "n1" } 
                     <| DynamicDiagram.addNode { name = "n1" }
                      <| DynamicDiagram.addImport { name = "n3" } { name = "n0" } 
                     <| DynamicDiagram.addNode { name = "n0" }
                      <| DynamicDiagram.addImport { name = "n4" } { name = "n3" } 
                     <| DynamicDiagram.addNode { name = "n3" }
                      <| DynamicDiagram.addImport { name = "n6" } { name = "n4" } 
                     <| DynamicDiagram.addNode { name = "n4" }
                     <| DynamicDiagram.addRootNode { name = "n6" }
                      <| DynamicDiagram.empty
          }, Cmd.none )

-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Tick newTime ->
      ( { model
        | diagram = DynamicDiagram.animate model.diagram
        } , Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.millisecond Tick

-- VIEW

viewboxMargin = 50 

          
view : Model -> Html Msg
view model =
    Html.div [  ]
      [ svg 
          [ version "1.1"
          , x "0", y "0",  SA.width "600", SA.height "500"
          , viewBox ( DynamicLayout.viewboxAsString viewboxMargin model.diagram.layout ) 
          ]
           ( DynamicDiagram.svgNodes model.diagram )
      ]

-- MAIN


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
