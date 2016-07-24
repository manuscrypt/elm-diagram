module Model.CompilationUnit exposing (..)

import Model.ElmFile exposing (ElmFile)
import Graph exposing (Graph, Node)


type CompilationStatus
    = Unknown
    | Compiling
    | Compiled
    | CompileError


type alias Model =
    { file : ElmFile
    , status : CompilationStatus
    }


type Msg
    = SetStatus CompilationStatus


init : ElmFile -> Model
init file =
    { file = file, status = Unknown }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetStatus status ->
            { model | status = status }


fromList : List ElmFile -> List Model
fromList strs =
    List.map init strs


fromElmFileGraph : Graph ElmFile () -> Graph Model ()
fromElmFileGraph elmFiles =
    Graph.mapNodes init elmFiles

labelFunc: Model -> String
labelFunc n = n.file.moduleName
