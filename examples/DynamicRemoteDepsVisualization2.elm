module DynamicRemoteDepsVisualization2 exposing (..)

import Window
import Html exposing (Html)
import Html.App as App
import Time exposing (Time, second)
import VirtualDom exposing (Node)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import SvgVisualization
import Extra.Svg as SvgUtils exposing (Stroke,bezierLineWithDirection,arrow)
import Color exposing ( Color )
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction )
import Task
import Http
import ElmFile exposing (ElmFile, decodeList)
import DynamicDiagram
import DynamicLayout
import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)
import ElmFileGraph exposing (fromFiles)
import IntDict

type alias Model =
    { message : String
    , graph : Graph ElmFile () 
    , files : List ElmFile
    , diagram : DynamicDiagram.Model 
    }

type Msg
    = Tick Time 
    | DataFetched (List ElmFile)
    | ErrorOccurred Http.Error
    | SetWindowSize Window.Size


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.batch [ Window.resizes SetWindowSize, Time.every Time.millisecond Tick ])
        }


init : ( Model, Cmd Msg )
init =
    { message = "Hello"
    , files = []
    , graph = Graph.empty
    , diagram = DynamicDiagram.empty
    } ! 
    [ ( Task.perform ErrorOccurred DataFetched <| Http.get decodeList "http://localhost:3001/" )         
    ]  

{-    
findElmFile : String -> List ElmFile -> ElmFile
findElmFile name files =
    let filtered = List.filter ( \file -> file.moduleName == name ) files
    in case filtered of 
    [ a ] -> a
    _ -> Debug.log "file not found" <| ElmFile.ElmFile 0 name name 0 [] name -- { id = 0, name = a, path = a, size = 0, imports = [], moduleName = a }    

addImports : ElmFile -> Model -> Model
addImports parent model = 
    List.foldr ( \importName m -> ( addImport parent importName m ) ) model parent.imports

addRootFile : ElmFile -> Model -> Model
addRootFile file model =
    addImports file 
        { model | diagram = DynamicDiagram.addRootNode { name = file.name } model.diagram }
    
addFile : ElmFile -> Model -> Model
addFile file model =
    addImports file 
        { model | diagram = DynamicDiagram.addNode { name = file.name } model.diagram }
-}
addImport : Graph.Node ElmFile -> Graph.Node ElmFile -> Model -> Model 
addImport parentNode importNode model =
    {-let importFile = -- Debug.log "imp" <|
     findElmFile importName model.files
    in -}
    let
        modelWithFile = model
        {-if( DynamicDiagram.containsNode { name = importFile.name } model.diagram )then
            model
        else
            addFile importFile model 
        -}
    in
        { modelWithFile | diagram =  DynamicDiagram.addImport 
            { name = parentNode.label.name } 
            { name = importNode.label.name } modelWithFile.diagram }
--        model

    
loadRoot : Graph.Node ElmFile -> Model -> Model
loadRoot root model =
    case Graph.get root.id model.graph of
        Nothing ->
            Debug.crash "ctx not found"
        Just ctx ->
            let induced = Debug.log "induces" <| 
                          Graph.inducedSubgraph ( (IntDict.keys ctx.outgoing)) model.graph
            in 
                List.foldl ( \node m -> (
                    addImport root node (
                        if( DynamicDiagram.containsNode { name = node.label.name } model.diagram )then
                            m
                        else 
                            { m | diagram = DynamicDiagram.addNode { name = node.label.name } m.diagram }
                    ) 
                    ) ) 
                    { model | diagram = DynamicDiagram.addRootNode { name = root.label.name } model.diagram } 
                    ( Graph.nodes induced )
--                            fromGraph induced model.size
 --xx = Debug.log "loadRoot" root
            --x = Debug.log "roots" <| roots
--        xxsadf = Debug.log "xxxx" root 
             --<| Graph.inducedSubgraph ( [ root.id ] ++ (IntDict.keys ctx.outgoing)) model.graph
--root 
        {- | Graph.guidedDfs
            Graph.alongIncomingEdges            -- which edges to follow
            ( Graph.onDiscovery (\ctx list ->    -- append node labels on discovery
                    ctx.node.label :: list))
            [ root ]            -- start with the node labelled "Shoes"
            []                                  -- accumulate starting with the empty list
            model.graph                  
            -}           -- traverse our dressUp graph from above
  --   in 
    -- addImports root
       
       --model

loadElmFiles : List ElmFile -> Model -> Model
loadElmFiles files model =
    let graph = fromFiles files
        edges = Debug.log "edges" <| Graph.edges graph
        modelWithGraph = { model 
                         | graph = graph
--                         , edges = edges                   
                         }
        nodes = Graph.nodes modelWithGraph.graph
        roots = List.filter ( \n -> n.label.name == "Basic.elm"       
           {- || ( n.label.name == "BasicElmDepsVisualization.elm" ) 
            || ( n.label.name == "DynamicLayoutSample1.elm" ) 
            || ( n.label.name == "DynamicLayoutSample2.elm" ) 
            || ( n.label.name == "DynamicRemoteDepsVisualization.elm" ) 
            || ( n.label.name == "RemoteDepsVisualization.elm" )
            -} 
            ) nodes
    in List.foldl ( \root m -> ( loadRoot root m ) ) modelWithGraph roots

    {-
    let
        g =
            
    in
        case Graph.get (idFor "Basic.elm" elmFiles) g of
            Nothing ->
                Debug.crash "no Basic.elm found"
            Just ctx ->
                let
                    induced =
                        Debug.log "oh"
                            <| Graph.inducedSubgraph ([ ctx.node.id ] ++ (IntDict.keys ctx.outgoing)) g
                in
                    fromGraph induced model.size
    let roots = List.filter ( \file -> ( ( file.name == "Basic.elm" )
 ) ) files
    in
    List.foldl addRootFile { model | files = files } roots
    -}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize size ->
            -- { model | size = size } ! []
            { model 
            | message = "SetWindowSize " ++ ( toString size ) 
            } ! []
        Tick newTime ->
            { model
            | diagram = DynamicDiagram.animate model.diagram
            , message =toString newTime 
            } ! []     
        ErrorOccurred err ->
            { model | message = toString err } ! []
        DataFetched elmFiles ->
            ( loadElmFiles elmFiles model ) ! []



view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ ] [ Html.text model.message ]
        , Html.div [ ] [ svg 
            [ version "1.1"
            , x "0", y "0",  SA.width "1600", SA.height "1500"
            , viewBox ( DynamicLayout.viewboxAsString 100 model.diagram.layout ) 
            ]
            ( DynamicDiagram.svgNodes model.diagram )
          ]
        ]
      

