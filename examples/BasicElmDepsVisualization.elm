module BasicElmDepsVisualization exposing (..)

import Html
import Html.App as App
import Html.Attributes as HA
import Color exposing (Color)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, setX, setY, sub)
import Svg exposing (Svg)
import Extra.Cmd exposing (noFx, updateOne, updateMany)
import Diagram exposing (..)
import Symbol
import CompilationUnit exposing (..)
import BasicElmDeps

rootCUs : List CompilationUnit.Model
rootCUs = 
    [ fst BasicElmDeps.init    
    , 
    fst BasicElmDeps.sample2
    ]

type alias Model =
    { diagram : Diagram.Model
    }


type Msg
    = NoOp
    | DiagramMsg Diagram.Msg


type alias LayoutNode = {
    pos : Vec2,
    color : Color
}

type alias LayoutCell = 
    { cu : CompilationUnit.Model 
    , index : Int
    }
{-
type alias LayoutRow = 
    { cells : List LayoutCell        
    }
-}
type alias Layout = 
    --{ rows : List LayoutRow
    --{ cus : List CompilationUnit.Model
    {
        cells: List LayoutCell
        --Dict.Dict CompilationUnit.Model LayoutCell
    }

addCu : CompilationUnit.Model -> Layout -> Layout
addCu cu layout =
    let newIndex = List.length layout.cells
        newCell = LayoutCell cu newIndex
        withCu = { layout | cells = layout.cells ++ [ newCell ] }
        (Dependencies deps) = cu.deps
    in List.foldl addCu withCu deps    

addRootCu : CompilationUnit.Model -> Layout -> Layout
addRootCu cu layout = 
    addCu cu ( Debug.log "addRootCu" layout )     

addRootCus : List CompilationUnit.Model -> Layout -> Layout
addRootCus cus layout = 
    List.foldl addRootCu layout cus

createNode : Int -> LayoutCell -> LayoutNode
createNode index cell = 
    let x = 30 + ( ( toFloat index ) * 75 )
        y = 330 - ( ( toFloat index ) * 25 )
    in LayoutNode ( vec2 x y ) Color.red 

getNodes : Layout -> List LayoutNode
getNodes layout =
    List.indexedMap createNode layout.cells

createNodes layout = 
    List.map ( \node -> ( Diagram.AddNode node.color (vec2 20 20) node.pos ) ) ( getNodes layout )

getCell : CompilationUnit.Model -> Layout -> Maybe LayoutCell
getCell cu layout = 
    let cs = List.filter ( \cell -> cell.cu == cu ) layout.cells
    in case cs of 
    [a] -> Maybe.Just a 
    _ -> Maybe.Nothing

createConn : LayoutCell -> CompilationUnit.Model -> Layout -> Diagram.Msg
createConn fromCell toCu layout =
    let toCell = ( getCell toCu layout )
    in case toCell of
    Just t -> Diagram.Connect [ t.index, fromCell.index ] --fromCell.index toCell.index ]
    Nothing -> Diagram.Connect [ 0, 1 ] --fromCell.index toCell.index ]

createConnForDeps cell layout =
    let (Dependencies deps) = cell.cu.deps
    in 
    List.map ( \dep -> createConn cell dep layout ) deps

createConns layout = 
    List.concat ( List.map ( \cell -> createConnForDeps cell layout ) layout.cells )
            


init : ( Model, Cmd Msg )
init =
    let layout = addRootCus rootCUs ( Layout [] )
    in
    let diagramMsgs = List.map ( \a -> DiagramMsg a ) ( 
            ( createNodes layout ) 
            ++ ( createConns layout ) 
        )
        ( d, dx ) =
            Diagram.init

        m0 = ( { diagram = d}
             , Cmd.batch [ Cmd.map DiagramMsg dx ]
             )
    in
        updateMany ( diagramMsgs ) update m0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            noFx model

        DiagramMsg msg ->
            let
                ( d, fx ) =
                    Diagram.update msg model.diagram
            in
                ( { model | diagram = d }, Cmd.map DiagramMsg fx )

diagram : Model -> Svg Msg
diagram model =
    Html.div
        [ HA.style
            [ (,) "z-index" "1"
            , (,) "opacity" "1"
            ]
        ]
        [ App.map DiagramMsg <| Diagram.view model.diagram ]


view : Model -> Svg Msg
view model =
    Html.div [ bodyStyle ]
        [ Html.div []
            [ diagram model
            ]
        , Html.div [ HA.style [ (,) "clear" "both", (,) "position" "relative" ] ] [ Html.text <| toString model ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


bodyStyle : Html.Attribute a
bodyStyle =
    HA.style
        [ (,) "width" "920px"
        , (,) "margin" "auto"
        , (,) "border" "1px solid black"
        , (,) "background-color" "#EEEEEE"
        ]
