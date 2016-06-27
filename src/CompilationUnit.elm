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
      , deps: Dependencies
      }

type Dependencies = Dependencies (List Model)

type Msg
  = SetStatus CompilationStatus
  | AddDependencies Dependencies

init: String->(Model, Cmd Msg)
init filename =
  { filename = filename, status = Unknown, deps = (Dependencies []) } ! []

update: Msg -> Model-> (Model, Cmd Msg)
update msg model =
  case msg of
    SetStatus status ->
      { model | status = status } ! []

    AddDependencies (Dependencies deps) ->
      let (Dependencies oldDeps) = model.deps
      in { model | deps = Dependencies (oldDeps ++ deps) } ! []
