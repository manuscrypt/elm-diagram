module Layouts.Rules exposing (..)

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction)


type alias NodeWithPos nodeType =
    ( nodeType, Vec2 )



-- RULES


type alias Rule nodeType =
    NodeWithPos nodeType -> NodeWithPos nodeType -> List (NodeWithPos nodeType)


type alias ForOneRule nodeType =
    NodeWithPos nodeType -> List (NodeWithPos nodeType)



-- MODEL


type alias Model nodeType =
    { nodes : List nodeType
    , positions : List (NodeWithPos nodeType)
    , velocity : List (NodeWithPos nodeType)
    , forEachRules : List (Rule nodeType)
    , forTwoRules : List ( nodeType, Rule nodeType, nodeType )
    , forOneRules : List (ForOneRule nodeType)
    }


type Msg
    = NoOp


init : Model nodeType
init =
    { nodes = []
    , positions = []
    , velocity = []
    , forEachRules = []
    , forTwoRules = []
    , forOneRules = []
    }


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


noIntersection : Float -> NodeWithPos nodeType -> NodeWithPos nodeType -> List (NodeWithPos nodeType)
noIntersection minDistance ( elemA, posA ) ( elemB, posB ) =
    let
        diff =
            (Math.Vector2.sub posA posB)

        dist =
            (Math.Vector2.length diff)
    in
        if (dist > minDistance) then
            []
        else
            let
                norm =
                    if (dist < 0.0001) then
                        (vec2 1 1)
                    else
                        (Math.Vector2.normalize diff)

                factor =
                    0.01 + 0.1 * (minDistance - dist)
            in
                [ ( elemA, (Math.Vector2.scale factor norm) )
                , ( elemB, (Math.Vector2.scale -factor norm) )
                ]


isAbove : Float -> NodeWithPos nodeType -> NodeWithPos nodeType -> List (NodeWithPos nodeType)
isAbove minDistance ( elemA, posA ) ( elemB, posB ) =
    let
        ay =
            (Math.Vector2.getY posA)

        by =
            (Math.Vector2.getY posB)

        fact =
            0.1 + 0.1 * (minDistance - (by - ay))
    in
        if (fact < 0) then
            []
        else
            [ ( elemA, (vec2 0 -fact) )
            , ( elemB, (vec2 0 fact) )
            ]


hasSameY : Float -> NodeWithPos nodeType -> NodeWithPos nodeType -> List (NodeWithPos nodeType)
hasSameY factor ( elemA, posA ) ( elemB, posB ) =
    let
        ay =
            (Math.Vector2.getY posA)

        by =
            (Math.Vector2.getY posB)

        fact =
            factor * (ay - by)
    in
        [ ( elemA, (vec2 0 -fact) )
        , ( elemB, (vec2 0 fact) )
        ]


hasSameX : Float -> NodeWithPos nodeType -> NodeWithPos nodeType -> List (NodeWithPos nodeType)
hasSameX factor ( elemA, posA ) ( elemB, posB ) =
    let
        ay =
            (Math.Vector2.getX posA)

        by =
            (Math.Vector2.getX posB)

        fact =
            factor * (ay - by)
    in
        [ ( elemA, (vec2 -fact 0) )
        , ( elemB, (vec2 fact 0) )
        ]


snapToGridSub : Float -> Float -> Float
snapToGridSub gridSize pos =
    let
        soll =
            (toFloat <| round (pos / gridSize)) * gridSize

        f =
            0.05
    in
        if (soll < pos) then
            -f
        else if (soll > pos) then
            f
        else
            0.0


snapToGrid : Float -> NodeWithPos nodeType -> List (NodeWithPos nodeType)
snapToGrid gridSize ( node, pos ) =
    [ ( node
      , vec2 (snapToGridSub gridSize (Math.Vector2.getX pos))
            (snapToGridSub gridSize (Math.Vector2.getY pos))
      )
    ]


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


calcForEachForce : NodeWithPos nodeType -> NodeWithPos nodeType -> Rule nodeType -> List (NodeWithPos nodeType)
calcForEachForce ( nodeA, posA ) ( nodeB, posB ) rule =
    rule ( nodeA, posA ) ( nodeB, posB )


calcForEachForcesFor : NodeWithPos nodeType -> NodeWithPos nodeType -> List (Rule nodeType) -> List (NodeWithPos nodeType)
calcForEachForcesFor ( nodeA, posA ) ( nodeB, posB ) forEachRules =
    List.concat <| List.map (\rule -> calcForEachForce ( nodeA, posA ) ( nodeB, posB ) rule) forEachRules


calcForEachForces : List (NodeWithPos nodeType) -> List (Rule nodeType) -> List (NodeWithPos nodeType)
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


calcForTwoForces : Model nodeType -> List (NodeWithPos nodeType)
calcForTwoForces model =
    List.concat
        <| List.map
            (\( nodeA, rule, nodeB ) ->
                rule ( nodeA, positionOfNode nodeA model ) ( nodeB, positionOfNode nodeB model )
            )
            model.forTwoRules


combineForcesFor : nodeType -> Vec2 -> List (NodeWithPos nodeType) -> Vec2
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


applyForcesFor : nodeType -> Vec2 -> List (NodeWithPos nodeType) -> Vec2
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


combineForces : Model nodeType -> List (NodeWithPos nodeType) -> List (NodeWithPos nodeType)
combineForces model forces =
    List.map (\node -> ( node, combineForcesFor node (vec2 0 0) forces )) model.nodes


applyForces : Model nodeType -> List (NodeWithPos nodeType) -> List (NodeWithPos nodeType)
applyForces model velocity =
    List.map (\( node, oldPosition ) -> ( node, applyForcesFor node oldPosition velocity )) model.positions
