module Data.Vector2 exposing (Vector2, add, dotProduct, scaleBy)


type alias Vector2 =
    { x : Float
    , y : Float
    }


add : Vector2 -> Vector2 -> Vector2
add { x, y } vector =
    { x = x + vector.x
    , y = y + vector.y
    }


scaleBy : Vector2 -> Float -> Vector2
scaleBy { x, y } number =
    { x = x * number
    , y = y * number
    }


dotProduct : Vector2 -> Vector2 -> Vector2
dotProduct vector { x, y } =
    { x = vector.x * x
    , y = vector.y * y
    }
