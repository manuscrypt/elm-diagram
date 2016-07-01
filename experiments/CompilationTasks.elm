module CompilationUnit exposing (..)

import Html exposing (..)
import Process exposing (..)


--import Svg exposing (..)

import Time exposing (..)
import Task exposing (..)
import Task.Extra as Task exposing (..)
import Date exposing (..)


type alias Source =
    { path : String
    , lastState : CompileState
    }


type CompileState
    = None
    | Compiling
    | Successful
    | Failed String


type alias Compilate =
    { source : Source
    , started : Maybe Date
    , duration : Time
    , artifacts : List String
    , state : CompileState
    }


type Msg
    = Compile
    | CompilationStarted Date
    | CompileError String
    | CompileSuccess ()


init : Source -> ( Compilate, Cmd Msg )
init src =
    ( Compilate src Nothing 0 [] None, startCompilation )


startCompilation : Cmd Msg
startCompilation =
    Date.now
        |> performFailproof CompilationStarted


executeCompilation : Compilate -> Cmd Msg
executeCompilation model =
    Task.perform CompileError CompileSuccess <| Process.sleep 500



--Task.perform CompileError (CompileSuccess) (compileBad model.source.path)


compileGood : a -> b -> Task c b
compileGood path =
    Task.succeed


compileBad : String -> Task String a
compileBad path =
    (Task.fail <| "error on line 57 in file " ++ path)


update : Msg -> Compilate -> ( Compilate, Cmd Msg )
update msg model =
    case msg of
        CompilationStarted date ->
            ( { model | started = Just date }, executeCompilation model )

        Compile ->
            ( model, startCompilation )

        CompileSuccess _ ->
            { model | state = Successful } ! []

        CompileError str ->
            { model | state = Failed str } ! []


view : Compilate -> Html Msg
view model =
    div [] [ Html.text "o" ]
