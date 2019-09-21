module Data.Vector2 exposing (Vector2, add, angleBetween, dotProduct, normalize, scaleBy)


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


normalize : Vector2 -> Vector2
normalize vector =
    let
        length =
            (vector.x ^ 2 + vector.y ^ 2)
                |> sqrt
    in
    scaleBy vector (1 / length)


dotProduct : Vector2 -> Vector2 -> Float
dotProduct vector { x, y } =
    vector.x * x + vector.y * y


angleBetween : Vector2 -> Vector2 -> Float
angleBetween vector vector_ =
    atan2 (crossProduct vector vector_) (dotProduct vector vector_)
        |> (\radians -> (radians * 180) / pi)



-- HELPERS


crossProduct : Vector2 -> Vector2 -> Float
crossProduct vector { x, y } =
    vector.x * y - vector.y * x
