module CompilationUnit exposing (..)

import ElmFile exposing (ElmFile)

type alias Filename = ElmFile

type CompilationStatus
  = Unknown
  | Compiling
  | Compiled
  | CompileError

type alias Model =
      { filename: Filename
      , status: CompilationStatus
      }

type Msg
  = SetStatus CompilationStatus

init: ElmFile->Model
init filename =
  { filename = filename, status = Unknown } 

update: Msg -> Model->Model
update msg model =
  case msg of
    SetStatus status ->
      { model | status = status }

fromList: List ElmFile -> List Model
fromList strs = 
  List.map init strs