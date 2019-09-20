module Game.Life exposing (view)

import Svg exposing (Svg)
import Svg.Attributes as Attributes


view : Int -> Float -> Int -> List (Svg msg)
view lives containerHeight containerWidth =
    let
        height =
            containerHeight * 0.16

        width =
            (containerWidth // 5) // lives
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
