module BasicElmDeps exposing (..)

import CompilationUnit exposing (fromList)
import DependencyGraph exposing (Msg(..),fromList)

v x = CompilationUnit.init x 
e x y = (,) x y

init : DependencyGraph.Model CompilationUnit.Model
init =
    let
        attrs = v "elm-stuff/packages/elm-lang/html/1.0.0/src/Html/Attributes.elm"
        app = v "elm-stuff/packages/elm-lang/html/1.0.0/src/Html/App.elm"   
        html = v "elm-stuff/packages/elm-lang/html/1.0.0/src/Html.elm"
        basic = v "examples/Basic.elm"

    in DependencyGraph.fromList [attrs, app, html, basic] 
            |> DependencyGraph.update (AddEdges
                            [ e basic html
                            , e basic app
 --                           , e basic attrs
 --                           , e html attrs
                            ])
--            |> DependencyGraph.topoSort

sample2 : DependencyGraph.Model String
sample2 = 
--  21-->22
--    +->23
--       23-->24
--    +->24
--
-- 
    let
        s21 = "Sample21.elm"
        s22 = "Sample22.elm"
        s23 = "Sample23.elm"
        s24 = "Sample24.elm"

    in DependencyGraph.fromList [s21,s22,s23,s24] 
            |> DependencyGraph.update (AddEdges
                            [ e s21 s22
                            , e s21 s23
                            , e s23 s24
                            , e s21 s24
                            ])