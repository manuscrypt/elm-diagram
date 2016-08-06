module Extra.Mouse exposing (toScreenPos)

import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Mouse


toScreenPos : Mouse.Position -> Vec2 -> Vec2
toScreenPos { x, y } size =
    vec2
        (toFloat x - (getX size) / 2)
        ((getY size) / 2 - toFloat y)
