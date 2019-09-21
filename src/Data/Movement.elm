module Data.Movement exposing (Movement, down, left, leftUp, none, right, rightUp, up)

import Data.Vector2 as Vector2 exposing (Vector2)


type alias Movement =
    Vector2


none : Movement
none =
    { x = 0, y = 0 }


left : Movement
left =
    { x = -1, y = 0 }


right : Movement
right =
    { x = 1, y = 0 }


up : Movement
up =
    { x = 0, y = -1 }


down : Movement
down =
    { x = 0, y = 1 }


leftUp : Movement
leftUp =
    Vector2.add left up
        |> Vector2.normalize


rightUp : Movement
rightUp =
    Vector2.add right up
        |> Vector2.normalize
