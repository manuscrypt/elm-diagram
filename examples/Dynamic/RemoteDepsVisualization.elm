module Dynamic.RemoteDepsVisualization exposing (..)

import Window
import Html exposing (Html)
import Html.App as App
import Time exposing (Time, second)
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import Task
import Http
import Model.ElmFile as ElmFile exposing (ElmFile, decodeList)
import Dynamic.Diagram as DynamicDiagram
import Dynamic.Layout as DynamicLayout


type alias Model =
    { message : String
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
        , subscriptions = (\model -> Sub.batch [ Window.resizes SetWindowSize, Time.every (17 * Time.millisecond) Tick ])
        }


init : ( Model, Cmd Msg )
init =
    { message = "Hello"
    , files = []
    , diagram = DynamicDiagram.empty
    }
        ! [ (Task.perform ErrorOccurred DataFetched <| Http.get decodeList "http://localhost:3001/")
          ]


findElmFile : String -> List ElmFile -> ElmFile
findElmFile name files =
    let
        filtered =
            List.filter (\file -> file.moduleName == name) files
    in
        case filtered of
            [ a ] ->
                a

            _ ->
                Debug.log "file not found" <| ElmFile.ElmFile 0 name name 0 [] name



-- { id = 0, name = a, path = a, size = 0, imports = [], moduleName = a }


addImport : ElmFile -> String -> Model -> Model
addImport parentFile importName model =
    let
        importFile =
            -- Debug.log "imp" <|
            findElmFile importName model.files
    in
        let
            modelWithFile =
                if (DynamicDiagram.containsNode { name = importFile.name } model.diagram) then
                    model
                else
                    addFile importFile model
        in
            { modelWithFile | diagram = DynamicDiagram.addImport { name = parentFile.name } { name = importFile.name } modelWithFile.diagram }


addImports : ElmFile -> Model -> Model
addImports parent model =
    List.foldr (\importName m -> (addImport parent importName m)) model parent.imports


addRootFile : ElmFile -> Model -> Model
addRootFile file model =
    addImports file
        { model | diagram = DynamicDiagram.addRootNode { name = file.name } model.diagram }


addFile : ElmFile -> Model -> Model
addFile file model =
    addImports file
        { model | diagram = DynamicDiagram.addNode { name = file.name } model.diagram }


loadElmFiles : List ElmFile -> Model -> Model
loadElmFiles files model =
    let
        roots =
            List.filter
                (\file ->
                    ((file.name == "Basic.elm")
                        || (file.name == "BasicDiagram.elm")
                        || (file.name == "Sample1.elm")
                        || (file.name == "Sample2.elm")
                        || (file.name == "RemoteDepsVisualization.elm")
                    )
                )
                files
    in
        List.foldl addRootFile { model | files = files } roots


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize size ->
            -- { model | size = size } ! []
            { model
                | message = "SetWindowSize " ++ (toString size)
            }
                ! []

        Tick newTime ->
            { model
                | diagram = DynamicDiagram.animate model.diagram
                , message = toString newTime
            }
                ! []

        ErrorOccurred err ->
            { model | message = toString err } ! []

        DataFetched elmFiles ->
            (loadElmFiles elmFiles model) ! []


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text model.message ]
        , Html.div []
            [ svg
                [ version "1.1"
                , x "0"
                , y "0"
                , SA.width "1600"
                , SA.height "1500"
                , viewBox (DynamicLayout.viewboxAsString 100 model.diagram.layout)
                ]
                (DynamicDiagram.svgNodes model.diagram)
            ]
        ]
