module MathVector2Utils exposing ( .. )

import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, sub, direction )

rotate : Vec2 -> Float -> Vec2
rotate v g =
    let ix = getX v
        iy = getY v
        r = g * 0.017453293
        s = sin r
        c = cos r
        sx = s * ix
        cx = c * ix
        sy = s * iy
        cy = c * iy
        tx = cx - sy
        ty = sx + cy
    in vec2 tx ty

