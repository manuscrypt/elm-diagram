module DepSampleSelf exposing (..)

import Html.App
import Html
import CompilationUnit exposing (..)

main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        }

attrs = fst <| CompilationUnit.init  "elm-stuff/packages/elm-lang/html/1.0.0/src/Html/Attributes.elm"
app = fst <| CompilationUnit.init "elm-stuff/packages/elm-lang/html/1.0.0/src/Html/App.elm"
html = fst <| CompilationUnit.init "elm-stuff/packages/elm-lang/html/1.0.0/src/Html.elm"

html' = fst <| CompilationUnit.update (AddDependencies (Dependencies [attrs])) html
html'' = fst <| CompilationUnit.update (AddDependencies (Dependencies [app])) html'

init =
   let (c0, c0x) = CompilationUnit.init "examples/Basic.elm"
       (withDeps, withDepsFx) = CompilationUnit.update (AddDependencies (Dependencies [html''])) c0
   in withDeps ! ([ c0x, withDepsFx ])

update msg model =
    model ! []

view model =
  Html.div [] [Html.text <| toString model]
