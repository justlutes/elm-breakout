module Game.Paddle exposing (Model, view)

import Data.Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type alias Model =
    { height : Float
    , width : Float
    }


view : Vector2 -> Model -> Svg msg
view vector { height, width } =
    Svg.rect
        [ Attributes.width <| String.fromFloat width
        , Attributes.height <| String.fromFloat height
        , Attributes.fill "#C64947"
        , Attributes.x <| String.fromFloat vector.x
        , Attributes.y <| String.fromFloat vector.y
        ]
        []
