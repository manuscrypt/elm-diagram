module Model.CompilationUnit exposing (..)

import Model.ElmFile exposing (ElmFile)
import Graph exposing (Graph, Node)


type alias Filename =
    ElmFile


type CompilationStatus
    = Unknown
    | Compiling
    | Compiled
    | CompileError


type alias Model =
    { filename : Filename
    , status : CompilationStatus
    }


type Msg
    = SetStatus CompilationStatus


init : ElmFile -> Model
init filename =
    { filename = filename, status = Unknown }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetStatus status ->
            { model | status = status }


fromList : List ElmFile -> List Model
fromList strs =
    List.map init strs


fromElmFileGraph : Graph ElmFile () -> ( Graph Model (), Node Model -> String )
fromElmFileGraph elmFiles =
    let
        basic =
            Graph.nodes elmFiles
                |> Debug.log "elms"
                |> List.filter (\n -> n.label.name == "Basic.elm")
                |> List.head
    in
        case basic of
            Nothing ->
                Debug.crash "No Basic.elm found"

            Just b ->
                let
                    smaller =
                        Graph.inducedSubgraph [ b.id ] elmFiles
                in
                    ( Graph.mapNodes init smaller
                    , (\a -> a.label.filename.name)
                    )
