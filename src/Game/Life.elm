module Game.Life exposing (view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Int -> Float -> Int -> List (Svg msg)
view lives containerHeight containerWidth =
    let
        height =
            containerHeight / toFloat 5

        width =
            (containerWidth // 4) // lives
    in
    List.range 1 lives
        |> List.map
            (\i ->
                Svg.rect
                    [ Attributes.height <| String.fromFloat height
                    , Attributes.width <| String.fromInt width
                    , Attributes.y <| String.fromFloat (containerHeight / 2 - (height / 2))
                    , Attributes.x <| String.fromInt (containerWidth - (width * i + (width // 2 * i)))
                    , Attributes.rx "1.25"
                    , Attributes.fill "#C64947"
                    ]
                    []
            )
