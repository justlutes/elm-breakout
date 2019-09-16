module Brick exposing (Model, getBrick, getRow, init, view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


type alias Model =
    { fill : String
    , density : Int
    , value : Int
    , positionY : Int
    , positionX : Int
    }


init : Int -> List Model
init rows =
    List.range 1 rows
        |> List.map create
        |> List.concat


create : Int -> List Model
create row =
    toList (toFillColor row)
        (floor (1 / toFloat row))
        (row * 2)
        (row * 3)


toFillColor : Int -> String
toFillColor i =
    String.concat
        [ "rgba(26, 188, 156, "
        , String.fromFloat <| 1 / toFloat i
        , ")"
        ]


toList : String -> Int -> Int -> Int -> List Model
toList color density value positionY =
    List.map (Model color density value positionY) (List.map ((*) 10) (List.range 0 9))


view : List Model -> List (Svg msg)
view bricks =
    List.map viewBrick bricks


viewBrick : Model -> Svg msg
viewBrick { fill, positionY, positionX } =
    Svg.rect
        [ Attributes.width "10%"
        , Attributes.height "3"
        , Attributes.fill fill
        , Attributes.y <| String.fromInt positionY
        , Attributes.x <| String.fromInt positionX ++ "%"
        ]
        []


getRow : Int -> List Model -> List Model
getRow ballY bricks =
    bricks
        |> List.filter (\block -> block.positionY == ballY)


getBrick : Int -> List Model -> Maybe Model
getBrick ballX bricks =
    bricks
        |> List.filter (\b -> b.positionX == ballX)
        |> List.head
