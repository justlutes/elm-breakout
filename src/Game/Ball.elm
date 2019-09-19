module Game.Ball exposing (view)

import Data.Vector2 exposing (Vector2)
import Html exposing (Html)
import Svg
import Svg.Attributes as Attributes


view : Vector2 -> Int -> Html msg
view { x, y } radius =
    Svg.circle
        [ Attributes.r <| String.fromInt radius
        , Attributes.fill "#C64947"
        , Attributes.cx <| String.fromFloat x
        , Attributes.cy <| String.fromFloat y
        ]
        []
