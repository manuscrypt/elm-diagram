module Dynamic.Layout2 exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)
import IntDict 
import Graph exposing (Graph, Node, NodeId, Edge, Adjacency)

-- MODEL

type alias Model n e =
    { graph : Graph n e
    , positions : IntDict.IntDict  Vec2
    }

loadGraph : Graph n e -> Model n e 
loadGraph graph = 
    { graph = graph
    , positions = IntDict.fromList <| List.map ( \n -> ( n.id, ( vec2 100 100 ) ) ) ( Graph.nodes graph )
    }

position : Int -> Model n e -> Vec2 
position index model = 
    case IntDict.get index model.positions of
        Just p -> p
        Nothing -> 
            let l = Debug.log "DynamicLayout2.no pos for id" index 
            in ( vec2 0 0 )

animate : Model n e -> Model n e
animate layout = layout
    -- hier kommt die magic

{-

type alias Viewbox =
    { left : Float
    , top : Float
    , right : Float 
    , bottom : Float
    }
    , viewbox : Viewbox
    , viewbox = Viewbox 0 0 0 0


empty : Model n e
empty =
    { graph = nodes = []
    , positions = []
    , velocity = []
    , forEachRules = []
    , forTwoRules = []
    , forOneRules = []
    }


addNode : nodeType -> Model nodeType -> Model nodeType
addNode node model =
    let
        r =
            (toFloat (List.length model.nodes)) * 0.001
    in
        addNodeAtPosition node (vec2 r (r * r * 100)) model


addNodeAtPosition : nodeType -> Vec2 -> Model nodeType -> Model nodeType
addNodeAtPosition node position model =
    { model
        | nodes = model.nodes ++ [ node ]
        , positions = model.positions ++ [ ( node, position ) ]
    }


positionOfNode : nodeType -> Model nodeType -> Vec2
positionOfNode node model =
    case List.filter (\( n, p ) -> n == node) model.positions of
        [ ( n, p ) ] ->
            p

        _ ->
            let
                d =
                    Debug.log "DynamicLayout.posOfNode node not found" node
            in
                (vec2 0 0)


addForEachRule : Rule nodeType -> Model nodeType -> Model nodeType
addForEachRule rule model =
    { model
        | forEachRules = model.forEachRules ++ [ rule ]
    }


addForTwoRule : nodeType -> Rule nodeType -> nodeType -> Model nodeType -> Model nodeType
addForTwoRule nodeA rule nodeB model =
    { model
        | forTwoRules = model.forTwoRules ++ [ ( nodeA, rule, nodeB ) ]
    }


addForOneRule : ForOneRule nodeType -> Model nodeType -> Model nodeType
addForOneRule rule model =
    { model
        | forOneRules = model.forOneRules ++ [ rule ]
    }


viewbox : Float -> Model nodeType -> ( Float, Float, Float, Float )
viewbox margin model =
    let
        positionsX =
            (List.map (\( n, pos ) -> (getX pos)) model.positions)

        minX =
            (Maybe.withDefault 0 (List.minimum positionsX)) - margin

        maxX =
            (Maybe.withDefault 0 (List.maximum positionsX)) + margin

        positionsY =
            (List.map (\( n, pos ) -> (getY pos)) model.positions)

        minY =
            (Maybe.withDefault 0 (List.minimum positionsY)) - margin

        maxY =
            (Maybe.withDefault 0 (List.maximum positionsY)) + margin
    in
        ( minX, minY, maxX, maxY )


viewboxAsString : Float -> Model nodeType -> String
viewboxAsString margin model =
    let
        ( minX, minY, maxX, maxY ) =
            viewbox margin model
    in
        (toString minX) ++ " " ++ (toString minY) ++ " " ++ (toString (maxX - minX)) ++ " " ++ (toString (maxY - minY))


animate : Model nodeType -> Model nodeType
animate model =
    let
        newForces =
            combineForces model
                ((calcForEachForces model.positions model.forEachRules)
                    ++ (calcForTwoForces model)
                    ++ (List.concat <| List.map (\( node, pos ) -> List.concat <| List.map (\rule -> rule ( node, pos )) model.forOneRules) model.positions)
                    ++ model.velocity
                )

        newPositions =
            applyForces model newForces

        newVelocity =
            List.map (\( node, vel ) -> ( node, Math.Vector2.scale 0.2751 vel )) newForces
    in
        { model
            | positions = newPositions
            , velocity = newVelocity
        }


calcForEachForce : ( nodeType, Vec2 ) -> ( nodeType, Vec2 ) -> Rule nodeType -> List ( nodeType, Vec2 )
calcForEachForce ( nodeA, posA ) ( nodeB, posB ) rule =
    rule ( nodeA, posA ) ( nodeB, posB )


calcForEachForcesFor : ( nodeType, Vec2 ) -> ( nodeType, Vec2 ) -> List (Rule nodeType) -> List ( nodeType, Vec2 )
calcForEachForcesFor ( nodeA, posA ) ( nodeB, posB ) forEachRules =
    List.concat <| List.map (\rule -> calcForEachForce ( nodeA, posA ) ( nodeB, posB ) rule) forEachRules


calcForEachForces : List ( nodeType, Vec2 ) -> List (Rule nodeType) -> List ( nodeType, Vec2 )
calcForEachForces nodeAndPosList forEachRules =
    case (List.head nodeAndPosList) of
        Nothing ->
            []

        Just a ->
            let
                others =
                    (List.drop 1 nodeAndPosList)
            in
                List.concat ((List.map (\b -> calcForEachForcesFor a b forEachRules) others))
                    ++ (calcForEachForces others forEachRules)


calcForTwoForces : Model nodeType -> List ( nodeType, Vec2 )
calcForTwoForces model =
    List.concat
        <| List.map
            (\( nodeA, rule, nodeB ) ->
                rule ( nodeA, positionOfNode nodeA model ) ( nodeB, positionOfNode nodeB model )
            )
            model.forTwoRules


combineForcesFor : nodeType -> Vec2 -> List ( nodeType, Vec2 ) -> Vec2
combineForcesFor node oldPosition forces =
    List.foldl
        (\( anyNode, force ) pos ->
            if (anyNode == node) then
                (Math.Vector2.add pos force)
            else
                pos
        )
        oldPosition
        forces


applyForcesFor : nodeType -> Vec2 -> List ( nodeType, Vec2 ) -> Vec2
applyForcesFor node oldPosition forces =
    List.foldl
        (\( anyNode, force ) pos ->
            if (anyNode == node) then
                (Math.Vector2.add pos force)
            else
                pos
        )
        oldPosition
        forces


combineForces : Model nodeType -> List ( nodeType, Vec2 ) -> List ( nodeType, Vec2 )
combineForces model forces =
    List.map (\node -> ( node, combineForcesFor node (vec2 0 0) forces )) model.nodes


applyForces : Model nodeType -> List ( nodeType, Vec2 ) -> List ( nodeType, Vec2 )
applyForces model velocity =
    List.map (\( node, oldPosition ) -> ( node, applyForcesFor node oldPosition velocity )) model.positions

-}