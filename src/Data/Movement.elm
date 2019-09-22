module Data.Movement exposing (down, left, leftUp, none, right, rightUp, up)

import Data.Vector2 as Vector2 exposing (Vector2)


none : Vector2
none =
    { x = 0, y = 0 }


left : Vector2
left =
    { x = -1, y = 0 }


right : Vector2
right =
    { x = 1, y = 0 }


up : Vector2
up =
    { x = 0, y = -1 }


down : Vector2
down =
    { x = 0, y = 1 }


leftUp : Vector2
leftUp =
    Vector2.add left up


rightUp : Vector2
rightUp =
    Vector2.add right up
