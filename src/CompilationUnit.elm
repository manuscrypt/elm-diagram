module CompilationUnit exposing (..)

type alias Filename = String

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

init: String->Model
init filename =
  { filename = filename, status = Unknown } 

update: Msg -> Model->Model
update msg model =
  case msg of
    SetStatus status ->
      { model | status = status }

fromList: List String -> List Model
fromList strs = 
  List.map init strs