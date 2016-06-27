module BasicElmDeps exposing (..)

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

init : ( CompilationUnit.Model, Cmd Msg ) 
init =
   let (c0, c0x) = CompilationUnit.init "examples/Basic.elm"
       (withDeps, withDepsFx) = CompilationUnit.update (AddDependencies (Dependencies [html''])) c0
   in withDeps ! ([ c0x, withDepsFx ])

--  21-->22
--    +->23
--       23-->24
--    +->24
--
--  

sample24 = fst <| CompilationUnit.init "Sample24.elm"
sample23a = fst <| CompilationUnit.init "Sample23.elm"
sample23 = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample24])) sample23a
sample22 = fst <| CompilationUnit.init "Sample22.elm"
--sample22 = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample24])) sample22a
--sample24 = fst <| CompilationUnit.init "Sample24.elm"
--sample21a = fst <| CompilationUnit.init "Sample21.elm"
--sample21b = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample22])) sample21a
--sample21c = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample24])) sample21b
sample21a = fst <| CompilationUnit.init "Sample21.elm"
sample21b = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample22])) sample21a
sample21 = fst <| CompilationUnit.update (AddDependencies (Dependencies [sample23])) sample21b

sample2 : ( CompilationUnit.Model, Cmd Msg ) 
sample2 =
   let (c0, c0x) = CompilationUnit.init "examples/Sample2.elm"
       (withDeps, withDepsFx) = CompilationUnit.update (AddDependencies (Dependencies [sample21])) c0
   in withDeps ! ([ c0x, withDepsFx ])

update msg model =
    model ! []

view model =
  Html.div [] [Html.text <| toString model]
