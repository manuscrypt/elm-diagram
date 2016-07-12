module Model.LayoutConfig exposing (..)


type alias Model node model =
    { addRootNode : node -> model -> model
    , addNode : node -> model -> model
    , containsNode : node -> model -> Bool
    , addImport : node -> node -> model -> model
    , animate : model -> model
    , model : model
    }


init :
    model
    -> (node -> model -> model)
    -> (node -> model -> model)
    -> (node -> model -> Bool)
    -> (node -> node -> model -> model)
    -> (model -> model)
    -> Model node model
init initialModel addRootNode addNode containsNode addImport animate =
    { addRootNode = addRootNode
    , addNode = addNode
    , containsNode = containsNode
    , addImport = addImport
    , animate = animate
    , model = initialModel
    }
