module Extra.Vec2Dict exposing
  ( Vec2Dict
  , empty
  , add, add2
  , get
  )


{-| # Vec2Dict

This module exposes a List of an Id, Vec2 pair.
Implemented by a IntDict.

-}

import Math.Vector2 as Vector2 exposing ( Vec2, vec2 )
import IntDict
import Maybe

{-| A dictionary mapping `Int`s to 'Vec2's. -}
type alias Vec2Dict = IntDict.IntDict Vec2

{-| Create an empty dictionary. -}
empty : Vec2Dict
empty = IntDict.empty

{-| Returns the Vec2 or 0 0 -}
vec2orNull : Maybe Vec2 -> Vec2
vec2orNull m =
  case m of
    Nothing -> vec2 0 0
    Just v -> v

{-| Add a Vec2 into the Dictionary.
If the Dictionary contains already this Id, the Vec2 will be added to his Vec2 -}
add : Int -> Vec2 -> Vec2Dict -> Vec2Dict
add index value dict =
  IntDict.update index ( \ existing ->
    case existing of
      Nothing -> Just value
      Just v -> Just <| Vector2.add v value
    ) dict

{-| Adds two Vec2 into the Dictionary.
Same as calling add twice. -}
add2 : ( Int, Vec2 ) -> ( Int, Vec2 ) -> Vec2Dict -> Vec2Dict
add2 ( ia, va ) ( ib, vb ) dict =
  add ia va <| add ib vb dict

{-| Get the value associated with a key. If the key is not found, return
vec 0 0. -}
get : Int -> Vec2Dict -> Vec2
get index dict =
  case ( IntDict.get index dict ) of
    Nothing -> vec2 0 0
    Just value -> value
