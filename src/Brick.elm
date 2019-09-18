module Brick exposing (Model, getBrick, getRow, init, view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


type alias Model =
    { fill : String
    , density : Int
    , positionY : Int
    , positionX : Float
    }


init : List (List Int) -> List Model
init rows =
    rows
        |> List.indexedMap create
        |> List.concat


create : Int -> List Int -> List Model
create row column =
    column
        |> List.indexedMap
            (\index density ->
                Model
                    (toFillColor density)
                    density
                    (20 * row + 40)
                    (100 / toFloat (List.length column) * toFloat index)
            )


toFillColor : Int -> String
toFillColor i =
    String.concat
        [ "rgba(26, 188, 156, "
        , String.fromFloat <| 1 / toFloat i
        , ")"
        ]


view : Int -> List Model -> List (Svg msg)
view columnLength bricks =
    let
        width =
            100 / toFloat columnLength
    in
    List.map (viewBrick width) bricks


viewBrick : Float -> Model -> Svg msg
viewBrick width { fill, positionY, positionX } =
    Svg.rect
        [ Attributes.width (String.fromFloat width ++ "%")
        , Attributes.height "20"
        , Attributes.stroke "rgb(237, 232, 225)"
        , Attributes.strokeWidth "1px"
        , Attributes.fill fill
        , Attributes.y <| String.fromInt positionY
        , Attributes.x <| String.fromFloat positionX ++ "%"
        ]
        []


getRow : Int -> List Model -> List Model
getRow ballY bricks =
    bricks
        |> List.filter (\block -> block.positionY == ballY)


getBrick : Float -> List Model -> Maybe Model
getBrick ballX bricks =
    bricks
        |> List.filter (\b -> b.positionX == ballX)
        |> List.head
